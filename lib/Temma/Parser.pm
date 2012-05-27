package Temma::Parser;
use strict;
use warnings;
no warnings 'utf8';
our $VERSION = '3.0';
use Whatpm::HTML::Defs;
use Whatpm::HTML::InputStream;
use Whatpm::HTML::Tokenizer;
push our @ISA, qw(Whatpm::HTML::Tokenizer);

sub parse_char_string ($$$;$$) {
  #my ($self, $string, $document, $onerror, $get_wrapper) = @_;
  my $self = ref $_[0] ? $_[0] : $_[0]->new;
  my $doc = $self->{document} = $_[2];
  @{$self->{document}->child_nodes} = ();

  ## Confidence: irrelevant.
  $self->{confident} = 1 unless exists $self->{confident};

  $self->{line_prev} = $self->{line} = 1;
  $self->{column_prev} = -1;
  $self->{column} = 0;

  $self->{chars} = [split //, $_[1]];
  $self->{chars_pos} = 0;
  $self->{chars_pull_next} = sub { 0 };
  delete $self->{chars_was_cr};

  my $onerror = $_[3] || $self->onerror;
  $self->{parse_error} = sub {
    $onerror->(line => $self->{line}, column => $self->{column}, @_);
  };

  $self->_initialize_tokenizer;
  $self->_initialize_tree_constructor;
  $self->{t} = $self->_get_next_token;
  $self->_construct_tree;
  $self->_terminate_tree_constructor;
  $self->_clear_refs;

  return $doc;
} # parse_char_string

## ------ Tree construction ------

sub HTML_NS () { q<http://www.w3.org/1999/xhtml> }
sub MML_NS () { q<http://www.w3.org/1998/Math/MathML> }
sub SVG_NS () { q<http://www.w3.org/2000/svg> }
sub TEMMA_NS () { q<http://suika.fam.cx/www/markup/temma> }

sub _initialize_tree_constructor ($) {
  my $self = shift;
  ## NOTE: $self->{document} MUST be specified before this method is called
  $self->{document}->strict_error_checking (0);
  ## TODO: Turn mutation events off # MUST
  $self->{document}->manakai_is_html (1);
  $self->{document}->set_user_data (manakai_source_line => 1);
  $self->{document}->set_user_data (manakai_source_column => 1);
} # _initialize_tree_constructor

sub _terminate_tree_constructor ($) {
  my $self = shift;
  $self->{document}->strict_error_checking (1);
  ## TODO: Turn mutation events on
} # _terminate_tree_constructor

## Tree construction stage

my @metavoid = qw(link meta base basefont bgsound command);
my @metacontent = qw(title style script);
my @bodyvoid = qw(
  area br embed img keygen wbr input param source track hr image isindex
  col
);

our $AutoOpen = {
  'html' => {
    '<text>' => 'body',
    'head' => '',
    'body' => '',
    '<start>' => 'body',
    (map { $_ => 'head' } @metavoid),
    (map { $_ => 'head' } @metacontent),
  },
  'table' => {
    'tr' => 'tbody',
  },
};

our $AutoClose = {
  head => {
    '<text>' => 1,
    '<start>' => 1,
    'head' => '',
    (map { $_ => '' } @metavoid),
    (map { $_ => '' } @metacontent),
  },
  tr => {
    thead => 1,
    tbody => 1,
    tfoot => 1,
  },
  th => {
    thead => 1,
    tbody => 1,
    tfoot => 1,
  },
  td => {
    thead => 1,
    tbody => 1,
    tfoot => 1,
  },
  colgroup => {
    thead => 1,
    tbody => 1,
    tfoot => 1,
    colgroup => 1,
    tr => 1,
  },
  option => {
    option => 1,
    optgroup => 1,
  },
  optgroup => {
    optgroup => 1,
  },
  rt => {
    rp => 1,
    rt => 1,
  },
  rp => {
    rp => 1,
    rt => 1,
  },
  't:else' => {
    '<text>' => 1,
    '<start>' => 1,
  },
};

our $Void = {
  (map { $_ => 1 } @metavoid),
  (map { $_ => 1 } @bodyvoid),
};

our $RawContent = {
  script => SCRIPT_DATA_STATE,
  style => RAWTEXT_STATE,
  textarea => RCDATA_STATE,
};

our $CloseIfInScope = {
  (map { $_ => {p => 1, body => -1} } qw(
    address article aside blockquote center details dialog dir div dl
    fieldset figcaption figure footer header hgroup menu nav ol
    section summary ul h1 h2 h3 h4 h5 h6 pre listing form plaintext
    form table hr isindex xmp
  )),
  p => {p => 1, body => -1},
  li => {li => 1, ol => -1, ul => -1, menu => -1, dir => -1, body => -1},
  dt => {dt => 1, dd => 1, dl => -1, body => -1},
  dd => {dt => 1, dd => 1, dl => -1, body => -1},
  'thead' => {'table' => 0},
  'tbody' => {'table' => 0},
  'tfoot' => {'table' => 0},
  'tr' => {'tr' => 1, 'table' => -1},
  'th' => {'th' => 1, 'td' => 1, 'table' => -1},
  'td' => {'th' => 1, 'td' => 1, 'table' => -1},
  't:else' => {'t:if' => 0},
};

our $EndTagOptional = {
  p => 1,
  li => 1,
  dt => 1,
  dd => 1,
  tr => 1,
  th => 1,
  td => 1,
  thead => 1,
  tbody => 1,
  tfoot => 1,
  rt => 1,
  rp => 1,
  option => 1,
  optgroup => 1,
};

sub IM_HTML () { 1 }
sub IM_SVG () { 2 }
sub IM_MML () { 3 }

sub _construct_tree ($) {
  my ($self) = @_;

  delete $self->{tainted};
  $self->{open_elements} = [];

  my $doctype = $self->{document}
      ->implementation->create_document_type ('html');
  $self->{document}->append_child ($doctype);

  my $el = $self->{document}->create_element_ns (HTML_NS, [undef, 'html']);
  $self->{document}->append_child ($el);
  push @{$self->{open_elements}}, [$el, 'html', IM_HTML];

  B: while (1) {
    if ($self->{t}->{type} == CHARACTER_TOKEN) {
      if ($self->{t}->{data} =~ s/^([\x09\x0A\x0C\x0D\x20]+)//) {
        $self->{open_elements}->[-1]->[0]->manakai_append_text ($1);
      }

      if (not length $self->{t}->{data}) {
        $self->{t} = $self->_get_next_token;
        next B;
      }

      if ($Void->{$self->{open_elements}->[-1]->[1]}) {
        pop @{$self->{open_elements}};
      }

      while (1) {
        my $ac = $AutoClose->{$self->{open_elements}->[-1]->[1]}->{'<text>'};
        if ($ac) {
          pop @{$self->{open_elements}};
        } else {
          last;
        }
      }
      
      while (1) {
        my $ao = $AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<text>'};
        if ($ao) {
          my $el = $self->{document}->create_element_ns
              (HTML_NS, [undef, $ao]);
          $self->{open_elements}->[-1]->[0]->append_child ($el);
          push @{$self->{open_elements}}, [$el, $ao, IM_HTML];
        } else {
          last;
        }
      }

      while ($self->{t}->{data} =~ s/\x00/\x{FFFD}/) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'NULL', token => $self->{t});
      }
      $self->{open_elements}->[-1]->[0]->manakai_append_text ($self->{t}->{data});
      
      $self->{t} = $self->_get_next_token;
      next B;
    } elsif ($self->{t}->{type} == START_TAG_TOKEN) {
      my $tag_name = $self->{t}->{tag_name};
      $tag_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.

      if ($tag_name eq 'html' or
          $tag_name eq 'head' or
          $tag_name eq 'body') {
        if (@{$self->{open_elements}} > 1 and
            not (@{$self->{open_elements}} == 2 and
                 $self->{open_elements}->[1]->[1] eq 'head' and
                 $tag_name eq 'body')) {
          $self->{parse_error}->(level => $self->{level}->{must},
                                 type => 'start_tag_not_allowed',
                                 text => $tag_name,
                                 token => $self->{t});

          delete $self->{self_closing};
          $self->{t} = $self->_get_next_token;
          next B;
        } elsif ($tag_name eq 'html') {
          my $el = $self->{open_elements}->[0]->[0];
          if ($el) {
            my $attrs = $self->{t}->{attributes};
            for my $attr_name (keys %{$attrs}) {
              next if $el->has_attribute_ns (undef, $attr_name);
              my $attr_t = $attrs->{$attr_name};
              my $attr = $self->{document}->create_attribute_ns
                  (undef, [undef, $attr_name]);
              $attr->value ($attr_t->{value});
              $attr->set_user_data (manakai_source_line => $attr_t->{line});
              $attr->set_user_data (manakai_source_column => $attr_t->{column});
              $el->set_attribute_node_ns ($attr);
            }
          }

          delete $self->{self_closing};
          $self->{t} = $self->_get_next_token;
          next B;
        }
      }

      if (not $tag_name =~ /^t:/ and
          $Void->{$self->{open_elements}->[-1]->[1]}) {
        pop @{$self->{open_elements}};
      }

      while (1) {
        my $ac = $AutoClose->{$self->{open_elements}->[-1]->[1]}->{$tag_name};
        $ac = $AutoClose->{$self->{open_elements}->[-1]->[1]}->{'<start>'}
            if not defined $ac;
        if ($ac) {
          pop @{$self->{open_elements}};
        } else {
          last;
        }
      }

      while (1) {
        my $ao = $AutoOpen->{$self->{open_elements}->[-1]->[1]}->{$tag_name};
        $ao = $AutoOpen->{$self->{open_elements}->[-1]->[1]}->{'<start>'}
            if not defined $ao;
        if ($ao) {
          my $el = $self->{document}->create_element_ns
              (HTML_NS, [undef, $ao]);
          $self->{open_elements}->[-1]->[0]->append_child ($el);
          push @{$self->{open_elements}}, [$el, $ao, IM_HTML];
        } else {
          last;
        }
      }

      my $cis = $CloseIfInScope->{$tag_name};
      if ($cis) {
        my $i = 0;
        for (reverse @{$self->{open_elements}}) {
          my $diff = $cis->{$_->[1]};
          if (defined $diff) {
            last if $diff < 0;
            my @closed = splice @{$self->{open_elements}},
                -($i + $diff), $i + $diff => ();
            shift @closed;
            my @not_closed = grep { not $EndTagOptional->{$_->[1]} } @closed;
            if (@not_closed) {
              $self->{parse_error}->(level => $self->{level}->{must},
                                     type => 'not closed',
                                     text => $not_closed[-1]->[1],
                                     token => $self->{t});
            }
            last;
          }
          $i++;
        }
      }

      my $im = $self->{open_elements}->[-1]->[2];
      my $ns = HTML_NS;
      my $local_name = $tag_name;
      my $attr_fixup = {};

      if ($local_name =~ s/^t:(?=.)//s) {
        $ns = TEMMA_NS;
      } elsif ($local_name eq 'svg') {
        $ns = SVG_NS;
        $im = IM_SVG;
        $attr_fixup = $Whatpm::HTML::ParserData::SVGAttrNameFixup;
      } elsif ($local_name eq 'math') {
        $ns = MML_NS;
        $im = IM_MML;
        $attr_fixup = $Whatpm::HTML::ParserData::MathMLAttrNameFixup;
      } else {
        if ($self->{open_elements}->[-1]->[2] == IM_SVG) {
          $ns = SVG_NS;
          $local_name = $Whatpm::HTML::ParserData::SVGElementNameFixup
              ->{$local_name} || $local_name;
          $attr_fixup = $Whatpm::HTML::ParserData::SVGAttrNameFixup;
        } elsif ($self->{open_elements}->[-1]->[2] == IM_MML) {
          $ns = MML_NS;
          $attr_fixup = $Whatpm::HTML::ParserData::MathMLAttrNameFixup;
        }
      }

      my $el = $self->{document}->create_element_ns
          ($ns, [undef, $local_name]);
      $el->set_user_data (manakai_source_line => $self->{t}->{line});
      $el->set_user_data (manakai_source_column => $self->{t}->{column});

      my $attrs = $self->{t}->{attributes};
      for my $attr_name (sort {$attrs->{$a}->{index} <=> $attrs->{$b}->{index}}
                         keys %{$attrs}) {
        my $attr;
        my $attr_t = $attrs->{$attr_name};
        my $nsfix = $Whatpm::HTML::ParserData::ForeignAttrNamespaceFixup
            ->{$attr_name};
        if ($nsfix) {
          $attr = $self->{document}->create_attribute_ns (@$nsfix);
        } elsif ($attr_name =~ s/^t:(?=.)//s) {
          $attr = $self->{document}->create_attribute_ns
              (TEMMA_NS, ['t', $attr_name]);
        } else {
          $attr = $self->{document}->create_attribute_ns
              (undef, [undef, $attr_fixup->{$attr_name} || $attr_name]);
        }
        $attr->value ($attr_t->{value});
        $attr->set_user_data (manakai_source_line => $attr_t->{line});
        $attr->set_user_data (manakai_source_column => $attr_t->{column});
        $el->set_attribute_node_ns ($attr);
      }

      $self->{open_elements}->[-1]->[0]->append_child ($el);

      if ($self->{self_closing}) {
        delete $self->{self_closing};
      } else {
        push @{$self->{open_elements}}, [$el, $tag_name, $im];

        if ($RawContent->{$tag_name}) {
          $self->{state} = $RawContent->{$tag_name};
          delete $self->{escape};
        }
      }
      
      $self->{t} = $self->_get_next_token;
      next B;

    } elsif ($self->{t}->{type} == END_TAG_TOKEN) {
      my $tag_name = $self->{t}->{tag_name};
      $tag_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.

      if ($tag_name eq 'html' or $tag_name eq 'body') {
        #
      } elsif ($tag_name eq 'sarcasm') {
        my $sarcasm = $self->{document}->create_element_ns
            (HTML_NS, [undef, 'sarcasm']);
        $self->{open_elements}->[-1]->[0]->append_child ($sarcasm);
      } elsif ($self->{open_elements}->[-1]->[1] eq $tag_name) {
        pop @{$self->{open_elements}};
      } else {
        ## Has an element in scope
        INSCOPE: {
          for my $i (reverse 0..$#{$self->{open_elements}}) {
            if ($self->{open_elements}->[$i]->[1] eq $tag_name) {
              my @closed = splice @{$self->{open_elements}}, $i;
              shift @closed;
              @closed = grep { not $EndTagOptional->{$_->[1]} } reverse @closed;
              if (@closed) {
                $self->{parse_error}->(level => $self->{level}->{must},
                                       type => 'not closed',
                                       text => $closed[-1]->[1],
                                       token => $self->{t});
              }
              last INSCOPE;
            }
          }

          $self->{parse_error}->(level => $self->{level}->{must},
                                 type => 'unmatched end tag',
                                 text => $self->{t}->{tag_name},
                                 token => $self->{t});
        } # INSCOPE
      }
      
      $self->{t} = $self->_get_next_token;
      redo B;
    } elsif ($self->{t}->{type} == COMMENT_TOKEN) {
      my $comment = $self->{document}->create_comment ($self->{t}->{data});
      $self->{open_elements}->[-1]->[0]->append_child ($comment);
      
      $self->{t} = $self->_get_next_token;
      next B;
    } elsif ($self->{t}->{type} == END_OF_FILE_TOKEN) {
      $self->{t} = {type => ABORT_TOKEN};
      return;
    } elsif ($self->{t}->{type} == DOCTYPE_TOKEN) {
      $self->{t} = $self->_get_next_token;
      next B;
    } elsif ($self->{t}->{type} == ABORT_TOKEN) {
      return;
    } else {
      die "Unknown token type $self->{t}->{type}";
    }
  } # B
} # _tree_in_element

1;
