package Temma::Parser;
use strict;
use warnings;
no warnings 'utf8';
our $VERSION = '4.0';
use Web::Temma::Tokenizer;
use Temma::Defs;
push our @ISA, qw(Web::Temma::Tokenizer);

sub IM_HTML () { 1 }
sub IM_SVG () { 2 }
sub IM_MML () { 3 }

sub parse_char_string ($$$) {
  my $self = shift;
  my $args = \@_;

  my @prefix;
  my $delta = 0;
  $self->{token_count} = 0;
  if ($self->{initial_state} and
      $self->{initial_state} eq 'body') {
    @prefix = split //, '<body>';
    $delta += -@prefix;
    $self->{token_count}--;
  }

  my $onerror = $self->onerror;
  local $self->{onerror};
  $self->onerror (sub {
    my %args = @_;
    require Web::HTML::SourceMap;
    my $di = $self->di;
    my $dids = $self->di_data_set;
    $dids->[$di]->{lc_map} ||= Web::HTML::SourceMap::create_index_lc_mapping
        ($args->[0]);
    my ($d, $i) = Web::HTML::SourceMap::resolve_index_pair
        ($dids, $args{di}, $args{index});
    ($args{line}, $args{column}) = Web::HTML::SourceMap::index_pair_to_lc_pair
        ($dids, $d, $i);
    $onerror->(%args);
  });
  $self->ontokens (\&_construct);

  delete $self->{tainted};
  $self->{open_elements} = [];

  my $doc = $args->[1];
  my $doctype = $doc->implementation->create_document_type ('html');
  my $el = $doc->create_element_ns (HTML_NS, [undef, 'html']);
  push @{$self->{open_elements}}, [$el, 'html', IM_HTML];
  my $strict = $doc->strict_error_checking;
  $doc->strict_error_checking (0);

  $self->SUPER::parse_char_string (@$args);

  $doc->strict_error_checking ($strict);
  $doc->append_child ($doctype);
  $doc->append_child ($el);
  delete $self->{open_elements};
} # parse_char_string

sub parse_f ($$$;$) {
  my ($self, $f => $doc, $onerror) = @_;
  require Encode;
  $self->parse_char_string
      (Encode::decode ('utf-8', scalar $f->slurp), # or die
       $doc, $onerror);
  $doc->set_user_data (manakai_source_f => $f);
  $doc->set_user_data (manakai_source_file_name => $f->stringify);
  return $doc;
} # parse_f

## ------ Tree construction ------

sub _construct ($$) {
  my ($self, $tokens) = @_;
  my $last_state;
  B: for my $token (@$tokens) {
    if ($token->{type} == Web::Temma::Tokenizer::TEXT_TOKEN) {
      if ($token->{value} =~ s/^([\x09\x0A\x0C\x0D\x20]+)//) {
        if ($self->{ignore_first_newline}) {
          my $v = $1;
          $v =~ s/^\x0A//;
          $self->{open_elements}->[-1]->[0]->manakai_append_content ($v)
              if length $v;
        } else {
          $self->{open_elements}->[-1]->[0]->manakai_append_content ($1);
        }
      }
      delete $self->{ignore_first_newline};

      if (not length $token->{value}) {
        next B;
      }

      $self->{token_count}++;
      if ($self->{open_elements}->[-1]->[2] == IM_HTML) {
        while (1) {
          my $ac = $Temma::Defs::AutoClose->{$self->{open_elements}->[-1]->[1]}->{'<text>'};
          if ($ac) {
            pop @{$self->{open_elements}};
          } else {
            last;
          }
        }
        
        while (1) {
          my $ao = $Temma::Defs::AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<text>'};
          $ao = $Temma::Defs::AutoOpen->{'<m>'}->{'<text>'}
              if not defined $ao and $self->{open_elements}->[-1]->[1] =~ /^m:/;
          $ao = $Temma::Defs::AutoOpen->{'<msg>'}->{'<text>'}
              if not defined $ao and $self->{open_elements}->[-1]->[1] =~ /^msg:/;
          if ($ao) {
            my $ln = $ao;
            my $el = $ln =~ s/^t://
                ? $self->{document}->create_element_ns
                      (TEMMA_NS, ['t', $ln])
                : $self->{document}->create_element_ns
                      (HTML_NS, [undef, $ln]);
            $self->{open_elements}->[-1]->[0]->manakai_append_content ($el);
            push @{$self->{open_elements}}, [$el, $ao, IM_HTML];
          } else {
            last;
          }
        }
      }

      $token->{value} =~ s/\x00/\x{FFFD}/g;
      $self->{open_elements}->[-1]->[0]->manakai_append_content
          ($token->{value});
      
      next B;
    } elsif ($token->{type} == Web::Temma::Tokenizer::START_TAG_TOKEN) {
      my $tag_name = $token->{tag_name};
      $tag_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
      delete $self->{ignore_first_newline};

      $self->{token_count}++;
      if ($tag_name eq 'head' or $tag_name eq 'body') {
        if (@{$self->{open_elements}} > 1 and
            not (@{$self->{open_elements}} == 2 and
                 $self->{open_elements}->[1]->[1] eq 'head' and
                 $tag_name eq 'body')) {
          $self->onerror->(level => 'm',
                           type => 'start tag not allowed',
                           text => $tag_name,
                           di => $token->{di}, index => $token->{index});

          delete $token->{self_closing_flag};
          next B;
        }
      } elsif ($tag_name eq 'html') {
        if ($self->{token_count} > 1) {
          $self->onerror->(level => 'm',
                           type => 'start tag not allowed',
                           text => 'html',
                           di => $token->{di}, index => $token->{index});
          delete $token->{self_closing_flag};
          next B;
        }

        my $allow_non_temma = @{$self->{open_elements}} <= 1;
        my $el = $self->{open_elements}->[0]->[0];
        if ($el) {
          my $attrs = $token->{attrs};
          for my $attr_name (keys %{$attrs}) {
            my $attr_t = $attrs->{$attr_name};
            if (not $attr_name =~ /^(?:t|m|msg|pl):/ and
                not $allow_non_temma) {
              $self->onerror->(level => 'm',
                               type => 'temma:html non temma attr',
                               text => $attr_name,
                               di => $token->{di}, index => $token->{index});
              next;
            }
            my $attr = $attr_name =~ s/^(t|m|msg|pl)://
                ? $self->{document}->create_attribute_ns
                    ($Temma::Defs::NamespacePrefixToURL->{$1},
                     [$1, $attr_name])
                : $self->{document}->create_attribute_ns
                    (undef, [undef, $attr_name]);
            if ($el->has_attribute_ns ($attr->namespace_uri, $attr->manakai_local_name)) {
              $self->onerror->(level => 'm',
                               type => 'duplicate attribute',
                               text => $attr->name,
                               di => $attr_t->{di}, index => $attr_t->{index});
              next;
            }
            $attr->manakai_append_indexed_string ($attr_t->{value});
            $attr->manakai_set_source_location (['', $attr_t->{di}, $attr_t->{index}]);
            $el->set_attribute_node_ns ($attr);
          } # $attr_name
          if (not $allow_non_temma and not keys %$attrs) {
            $self->onerror->(level => 'm',
                             type => 'start tag not allowed',
                             text => 'html',
                             di => $token->{di}, index => $token->{index});
          }
        } # $el
        
        delete $token->{self_closing_flag};
        next B;
      }

      if ($self->{open_elements}->[-1]->[2] == IM_HTML) {
        while (1) {
          my $ac = $Temma::Defs::AutoClose->{$self->{open_elements}->[-1]->[1]}->{$tag_name};
          $ac = $Temma::Defs::AutoClose->{$self->{open_elements}->[-1]->[1]}->{'<m>'}
              if not defined $ac and $tag_name =~ /^m:/;
          $ac = $Temma::Defs::AutoClose->{$self->{open_elements}->[-1]->[1]}->{'<start>'}
              if not defined $ac;
          if ($ac) {
            pop @{$self->{open_elements}};
          } else {
            last;
          }
        }

        while (1) {
          my $ao = $Temma::Defs::AutoOpen->{$self->{open_elements}->[-1]->[1]}->{$tag_name};
          $ao = $Temma::Defs::AutoOpen->{'<m>'}->{$tag_name}
              if not defined $ao and
                  $self->{open_elements}->[-1]->[1] =~ /^m:/;
          $ao = $Temma::Defs::AutoOpen->{'<msg>'}->{$tag_name}
              if not defined $ao and
                  $self->{open_elements}->[-1]->[1] =~ /^msg:/;
          $ao = $Temma::Defs::AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<m>'}
              if not defined $ao and $tag_name =~ /^m:/;
          $ao = $Temma::Defs::AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<msg>'}
              if not defined $ao and $tag_name =~ /^msg:/;
          $ao = $Temma::Defs::AutoOpen->{'<m>'}->{'<start>'}
              if not defined $ao and $self->{open_elements}->[-1]->[1] =~ /^m:/;
          $ao = $Temma::Defs::AutoOpen->{'<msg>'}->{'<start>'}
              if not defined $ao and $self->{open_elements}->[-1]->[1] =~ /^msg:/;
          $ao = $Temma::Defs::AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<start>'}
              if not defined $ao;
          if ($ao) {
            my $ln = $ao;
            my $el = $ln =~ s/^t://
                ? $self->{document}->create_element_ns
                      (TEMMA_NS, ['t', $ln])
                : $self->{document}->create_element_ns
                      (HTML_NS, [undef, $ln]);
            $self->{open_elements}->[-1]->[0]->manakai_append_content ($el);
            push @{$self->{open_elements}}, [$el, $ao, IM_HTML];
          } else {
            last;
          }
        }

        my $cis = $Temma::Defs::CloseIfInScope->{$tag_name};
        if ($cis) {
          my $i = 0;
          for (reverse @{$self->{open_elements}}) {
            my $diff = $cis->{$_->[1]};
            $diff = $cis->{'<m>'} if not defined $diff and $_->[1] =~ /^m:/;
            $diff = $cis->{'<msg>'} if not defined $diff and $_->[1] =~ /^msg:/;
            if (defined $diff) {
              last if $diff < 0;
              my @closed = splice @{$self->{open_elements}},
                  -($i + $diff), $i + $diff => ();
              my @not_closed = grep { not $Temma::Defs::EndTagOptional->{$_->[1]} } @closed;
              if (@not_closed) {
                $self->onerror->(level => 'm',
                                 type => 'not closed',
                                 text => $not_closed[-1]->[1],
                                 di => $token->{di}, index => $token->{index});
              }
              last;
            }
            $i++;
          }
        }
      }

      my $im = $self->{open_elements}->[-1]->[2];
      my $ns = HTML_NS;
      my $prefix;
      my $local_name = $tag_name;
      my $attr_fixup = {};

      if ($local_name =~ s/^(t|m|msg|pl):(?=.)//s) {
        $prefix = $1;
        $ns = $Temma::Defs::NamespacePrefixToURL->{$prefix};
      } elsif ($local_name eq 'svg') {
        $ns = SVG_NS;
        $im = IM_SVG;
        $attr_fixup = $Web::HTML::ParserData::SVGAttrNameFixup;
      } elsif ($local_name eq 'math' or
               $local_name eq 'mglyph' or
               $local_name eq 'malignmark') {
        $ns = MML_NS;
        $im = IM_MML;
        $attr_fixup = $Web::HTML::ParserData::MathMLAttrNameFixup;
      } else {
        if ($self->{open_elements}->[-1]->[2] == IM_SVG) {
          $ns = SVG_NS;
          $local_name = $Web::HTML::ParserData::SVGElementNameFixup
              ->{$local_name} || $local_name;
          $attr_fixup = $Web::HTML::ParserData::SVGAttrNameFixup;
        } elsif ($self->{open_elements}->[-1]->[2] == IM_MML) {
          $ns = MML_NS;
          $attr_fixup = $Web::HTML::ParserData::MathMLAttrNameFixup;
        }
      }

      my $el = $self->{document}->create_element_ns
          ($ns, [$prefix, $local_name]);
      $el->manakai_set_source_location (['', $token->{di}, $token->{index}]);

      my $attrs = $token->{attrs};
      for my $attr_name (sort {$attrs->{$a}->{index} <=> $attrs->{$b}->{index}}
                         keys %{$attrs}) {
        my $attr;
        my $attr_t = $attrs->{$attr_name};
        my $nsfix = $Web::HTML::ParserData::ForeignAttrNamespaceFixup
            ->{$attr_name};
        if ($nsfix) {
          $attr = $self->{document}->create_attribute_ns (@$nsfix);
        } elsif ($attr_name =~ s/^(t|m|msg|pl):(?=.)//s) {
          $attr_name = $attr_fixup->{$attr_name} || $attr_name
              if $1 eq 'msg' or $1 eq 'pl';
          $attr = $self->{document}->create_attribute_ns
              ($Temma::Defs::NamespacePrefixToURL->{$1},
               [$1, $attr_name]);
        } else {
          $attr = $self->{document}->create_attribute_ns
              (undef, [undef, $attr_fixup->{$attr_name} || $attr_name]);
        }
        $attr->manakai_append_indexed_string ($attr_t->{value});
        $attr->manakai_set_source_location (['', $attr_t->{di}, $attr_t->{index}]);
        $el->set_attribute_node_ns ($attr);
      } # $attrs

      $self->{open_elements}->[-1]->[0]->manakai_append_content ($el);

      if ($token->{self_closing_flag}) {
        delete $token->{self_closing_flag};
      } else {
        my $orig_im = $im;
        if ($ns eq SVG_NS) {
          if ($Web::HTML::ParserData::SVGHTMLIntegrationPoints->{$local_name}) {
            $im = IM_HTML;
          }
        } elsif ($ns eq MML_NS) {
          if ($Web::HTML::ParserData::MathMLTextIntegrationPoints->{$local_name} or
              $Web::HTML::ParserData::MathMLHTMLIntegrationPoints->{$local_name} or
              ($local_name eq 'annotation-xml' and
               $attrs->{encoding} and
               do {
                 my $enc = join '', map { $_->[0] } @{$attrs->{encoding}->{value}}; # IndexedString
                 $enc =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
                 ($enc eq 'text/html' or $enc eq 'application/xhtml+xml');
               })) {
            $im = IM_HTML;
          }
        }
        
        push @{$self->{open_elements}}, [$el, $tag_name, $im];

        if ($attrs->{'t:parse'}) {
          #
        } elsif (($orig_im == IM_SVG or $orig_im == IM_MML) and
                 not $tag_name =~ /^t:/ and not $tag_name =~ /^m:/) {
          #
        } elsif ($Temma::Defs::Void->{$tag_name}) {
          pop @{$self->{open_elements}};
        } elsif ($Temma::Defs::RawContent->{$tag_name}) {
          $last_state = $Temma::Defs::RawContent->{$tag_name};
          delete $self->{escape};
        }

        if ($ns eq HTML_NS and 
            $Temma::Defs::IgnoreFirstNewline->{$local_name}) {
          $self->{ignore_first_newline} = 1;
        }
      }
      
      next B;

    } elsif ($token->{type} == Web::Temma::Tokenizer::END_TAG_TOKEN) {
      my $tag_name = $token->{tag_name};
      $tag_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
      delete $self->{ignore_first_newline};

      if ($tag_name eq '') {
        if ($self->{open_elements}->[-1]->[1] eq 'html' or
            $self->{open_elements}->[-1]->[1] eq 'body') {
          $self->onerror->(level => 'm',
                           type => 'unmatched end tag',
                           text => $token->{tag_name},
                           di => $token->{di}, index => $token->{index});
        } else {
          pop @{$self->{open_elements}};
        }
      } elsif ($tag_name eq 'html' or $tag_name eq 'body') {
        #
      } elsif ($tag_name eq 'sarcasm') {
        my $sarcasm = $self->{document}->create_element_ns
            (HTML_NS, [undef, 'sarcasm']);
        $self->{open_elements}->[-1]->[0]->manakai_append_content ($sarcasm);
      } elsif ($self->{open_elements}->[-1]->[1] eq $tag_name) {
        pop @{$self->{open_elements}};
      } elsif ($tag_name =~ /\A(t|m|msg|pl):\z/) {
        my $ns = $Temma::Defs::NamespacePrefixToURL->{$1};
        
        ## Has an element in scope
        INSCOPE: {
          for my $i (reverse 0..$#{$self->{open_elements}}) {
            if (($self->{open_elements}->[$i]->[0]->namespace_uri || '') eq $ns) {
              my @closed = splice @{$self->{open_elements}}, $i;
              shift @closed;
              @closed = grep {
                not $Temma::Defs::EndTagOptional->{$_->[1]} or
                $_->[2] == IM_SVG or $_->[2] == IM_MML
              } reverse @closed;
              if (@closed) {
                $self->onerror->(level => 'm',
                                 type => 'not closed',
                                 text => $closed[-1]->[1],
                                 di => $token->{di}, index => $token->{index});
              }
              last INSCOPE;
            }
          }

          $self->onerror->(level => 'm',
                           type => 'unmatched end tag',
                           text => $token->{tag_name},
                           di => $token->{di}, index => $token->{index});
        } # INSCOPE
      } else {
        ## Has an element in scope
        INSCOPE: {
          for my $i (reverse 0..$#{$self->{open_elements}}) {
            if ($self->{open_elements}->[$i]->[1] eq $tag_name) {
              my @closed = splice @{$self->{open_elements}}, $i;
              shift @closed;
              @closed = grep {
                not $Temma::Defs::EndTagOptional->{$_->[1]} or
                $_->[2] == IM_SVG or $_->[2] == IM_MML
              } reverse @closed;
              if (@closed) {
                $self->onerror->(level => 'm',
                                 type => 'not closed',
                                 text => $closed[-1]->[1],
                                 di => $token->{di}, index => $token->{index});
              }
              last INSCOPE;
            }
          }

          $self->onerror->(level => 'm',
                           type => 'unmatched end tag',
                           text => $token->{tag_name},
                           di => $token->{di}, index => $token->{index});
        } # INSCOPE
      }
      
      next B;
    } elsif ($token->{type} == Web::Temma::Tokenizer::COMMENT_TOKEN) {
      my $comment = $self->{document}->create_comment ('');
      $comment->manakai_append_indexed_string ($token->{data});
      $self->{open_elements}->[-1]->[0]->manakai_append_content ($comment);
      
      delete $self->{ignore_first_newline};
      next B;
    } elsif ($token->{type} == Web::Temma::Tokenizer::END_OF_FILE_TOKEN) {
      last B;
    } elsif ($token->{type} == Web::Temma::Tokenizer::DOCTYPE_TOKEN) {
      delete $self->{ignore_first_newline};
      next B;
    } else {
      die "Unknown token type $token->{type}";
    }
  } # B
  return $last_state;
} # _construct

1;

=head1 LICENSE

Copyright 2012-2015 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
