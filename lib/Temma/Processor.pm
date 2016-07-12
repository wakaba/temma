package Temma::Processor;
use strict;
use warnings;
no warnings 'utf8';
#
sub _eval ($) {
  return eval ('local @_;' . "\n" . $_[0]);
} # _eval
#
our $VERSION = '5.0';
use Web::DOM::Node;
use Web::HTML::SourceMap;
use Temma::Defs;

sub new ($) {
  return bless {doc_to_path => {}}, $_[0];
} # new

sub onerror ($;$) {
  if (@_ > 1) {
    $_[0]->{onerror} = $_[1];
  }
  return $_[0]->{onerror} ||= do {
    my $dids = $_[0]->di_data_set;
    my $doc_to_path = $_[0]->{doc_to_path};
    sub {
      my $error = {@_};
      my $text = defined $error->{text} ? qq{ - $error->{text}} : '';
      my $value = defined $error->{value} ? qq{ "$error->{value}"} : '';
      my $level = {
        m => 'Parse error',
        s => 'SHOULD-level error',
        w => 'Warning',
        i => 'Information',
      }->{$error->{level} || ''} || $error->{level};
      my $node = '';
      if (defined $error->{node}) {
        $node = ' node ' . $error->{node}->node_name;
        my $sl = $error->{node}->manakai_get_source_location;
        $error->{di} = $sl->[1];
        $error->{index} = $sl->[2];
      }
      ($error->{di}, $error->{index}) = resolve_index_pair
          $dids, $error->{di}, $error->{index};
      ($error->{line}, $error->{column}) = index_pair_to_lc_pair
          $dids, $error->{di}, $error->{index};
      my $doc = 'document #' . $error->{di};
      if (not $error->{di} == -1) {
        #my $did = $dids->[$error->{di}];
        #if (defined $did->{name}) {
        #  $doc = $did->{name};
        #} elsif (defined $did->{url}) {
        #  $doc = 'document <' . $did->{url} . '>';
        #}
      }
      if (defined $error->{node}) {
        my $fn = $doc_to_path->{$error->{node}->owner_document or $error->{node}};
        $doc = 'path "' . $fn . '"' if defined $fn;
      }
      my $pos = "index $error->{index}";
      if (defined $error->{column}) {
        $pos = "line $error->{line} column $error->{column}";
      }
      warn "$level ($error->{type}$text) at $doc $pos$node$value\n";
    };
  };
} # onerror

sub oninclude ($;$) {
  if (@_ > 1) {
    $_[0]->{oninclude} = $_[1];
  }
  return $_[0]->{oninclude} ||= sub {
    my $x = $_[0];

    require Path::Class;
    my $base_f = $x->{doc_to_path}->{$x->{context}->owner_document};
    my $included_f = Path::Class::file ($x->{path});
    $included_f = $included_f->absolute ($base_f->dir) if $base_f;

    my $parser = $x->{get_parser}->();
    $parser->onerror (sub {
      $x->{onerror}->(@_, f => $included_f);
    });

    my $doc = $x->{create_document}->();
    $x->{doc_to_path}->{$doc} = $included_f;
    $parser->parse_f ($included_f => $doc);

    return $doc;
  };
} # oninclude

sub locale ($;$) {
  if (@_ > 1) {
    $_[0]->{locale} = $_[1];
  }
  return $_[0]->{locale};
} # locale

sub di_data_set ($;$) {
  if (@_ > 1) {
    $_[0]->{di_data_set} = $_[1];
  }
  return $_[0]->{di_data_set} ||= [];
} # di_data_set

sub htescape ($) {
  return $_[0] unless $_[0] =~ /[&"<>]/;
  my $s = $_[0];
  $s =~ s/&/&amp;/g;
  $s =~ s/"/&quot;/g;
  $s =~ s/</&lt;/g;
  $s =~ s/>/&gt;/g;
  return $s;
} # htescape

sub _ascii_lc ($) {
  my $s = $_[0];
  $s =~ tr/A-Z/a-z/ if defined $s; ## ASCII case-insensitive.
  return $s;
} # _ascii_lc

my $TemmaContextNode = sub ($) {
  my $node = $_[0];
  while (1) {
    my $url = $node->namespace_uri || '';
    if ($url eq TEMMA_NS) {
      my $parent = $node->parent_node;
      if ($parent) {
        $node = $parent;
      } else {
        last;
      }
    } else {
      last;
    }
  }
  return $node;
}; # $TemmaContextNode

sub process_document ($$$;%) {
  my ($self, $doc => $fh, %args) = @_;

  $self->{processes} = [];
  $self->{location_cache} = {};
  $self->{args} = $args{args} || {};
  $self->{doc} = $doc; ## Hold ref to Document to not destory until done
  push @{$self->{processes}},
      {type => 'node', node => $doc,
       node_info => {allow_children => 1}},
      {type => 'end', ondone => $args{ondone}};

  my $f = $doc->get_user_data ('manakai_source_f');
  if (UNIVERSAL::isa ($f, 'Path::Class::File')) {
    $self->{doc_to_path}->{$doc} = $f;
  }

  $self->_process ($fh);
} # process_document

sub process_fragment ($$$;%) {
  my ($self, $doc => $fh, %args) = @_;

  $self->{processes} = [];
  $self->{args} = $args{args} || {};
  $self->{doc} = $doc; ## Hold ref to Document to not destory until done

  if (my $body = $doc->body) {
    my $sp = {preserve => 'preserve', trim => 'trim'}->{_ascii_lc $body->get_attribute_ns (TEMMA_NS, 'space') || ''}
        || {preserve => 'preserve', trim => 'trim'}->{_ascii_lc $body->parent_node->get_attribute_ns (TEMMA_NS, 'space') || ''}
        || ($args{plain_text} ? 'preserve' : '');
    my $binds = {};
    my $node_info = {allow_children => 1,
                     preserve_space => $sp eq 'preserve',
                     binds => $binds,
                     plaintext => $args{plain_text}};
    $self->_get_params ($doc->document_element, bind_args => $binds);

    unshift @{$self->{processes}},
        map { {type => 'node', node => $_, node_info => $node_info} } 
        grep { $_->node_type == ELEMENT_NODE or
               $_->node_type == TEXT_NODE }
        @{$body->child_nodes->to_a};
  }

  push @{$self->{processes}},
      {type => 'end', ondone => $args{ondone}};

  my $f = $doc->get_user_data ('manakai_source_f');
  if (UNIVERSAL::isa ($f, 'Path::Class::File')) {
    $self->{doc_to_path}->{$doc} = $f;
  }

  $self->_process ($fh);
} # process_fragment

sub process_plain_text ($$$;%) {
  my $self = shift;
  $self->process_fragment (@_, plain_text => 1);
} # process_plain_text

sub _process ($$) {
  my ($self, $fh) = @_;
  A: {
    eval {
      $self->__process ($fh);
    };
    if ($@) {
      my $exception = $@;
      if (UNIVERSAL::isa ($exception, 'Temma::Exception')) {
        my $catch;
        my @close;
        while (@{$self->{processes}}) {
          my $process = shift @{$self->{processes}};
          if ($process->{type} eq 'end block' and
              $process->{catches}) {
            for my $c (@{$process->{catches}}) {
              if (not defined $c->{package} or
                  $exception->isa_package ($c->{package})) {
                $catch = $c;
                last;
              }
            }
            push @close, $process;
            last if $catch;
          } elsif ({
            end => 1, 'end tag' => 1, 'end block' => 1,
          }->{$process->{type}}) {
            push @close, $process;
          }
        }

        my $close = shift @close;
        unshift @{$self->{processes}}, @close;
        if ($catch) {
          my $binds = $catch->{node_info}->{binds} || {};
          if (defined $catch->{bound_to}) {
            $binds = {%$binds, $catch->{bound_to} => [[$exception], 0]};
          }
          $self->_schedule_nodes
              ($catch->{nodes}, $catch->{node_info}, $catch->{sp},
               binds => $binds);
        } else {
          #warn $exception->source_text;
          $self->onerror->(type => 'temma:perl exception',
                           level => 'm',
                           value => $exception,
                           node => $exception->source_node);
        }
        unshift @{$self->{processes}}, $close;
        redo A;
      } else {
        $self->_cleanup;
        die $exception;
      }
    }
  } # A
} # _process

sub __process ($$) {
  my ($self, $fh) = @_;

  while (@{$self->{processes}}) {
    my $process = shift @{$self->{processes}};

    if ($process->{type} eq 'node') {
      my $node = $process->{node};
      my $nt = $node->node_type;
      if ($nt == TEXT_NODE) {
        my $data = $node->data;
        if ($data =~ /[^\x09\x0A\x0C\x0D\x20]/) {
          ## Non white-space node
          next if $self->_close_start_tag ($process, $fh);
          if (not $process->{node_info}->{has_non_space} and 
              not $process->{node_info}->{preserve_space}) {
            $data =~ s/^[\x09\x0A\x0C\x0D\x20]+//;
            $process->{node_info}->{has_non_space} = 1;
          }
        } else {
          ## White space only Text node
          if ($process->{node_info}->{preserve_space}) {
            next if $self->_close_start_tag ($process, $fh);
          } elsif (not $process->{node_info}->{has_non_space}) {
            next;
          }
          if ($self->{current_tag}) {
            ## Ignore white space characters between t:attr elements.
            next;
          }
        }

        unless ($process->{node_info}->{allow_children}) {
          unless ($process->{node_info}->{children_not_allowed_error}) {
            $self->onerror->(type => 'temma:child not allowed',
                             node => $node,
                             level => 'm');
            $process->{node_info}->{children_not_allowed_error} = 1;
          }
          next;
        }

        if (defined $process->{node_info}->{trailing_space}) {
          $data = $process->{node_info}->{trailing_space} . $data;
          delete $process->{node_info}->{trailing_space};
        }

        unless ($process->{node_info}->{preserve_space}) {
          if ($data =~ s/([\x09\x0A\x0C\x0D\x20]+)\z//) {
            if (not defined $process->{node_info}->{trailing_space}) {
              $process->{node_info}->{trailing_space} = $1;
            } else {
              $process->{node_info}->{trailing_space} .= $1;
            }
          }
        }

        if (not length $data) {
          #
        } elsif ($process->{node_info}->{rawtext}) {
          ${$process->{node_info}->{rawtext_value}} .= $data;
        } elsif ($process->{node_info}->{plaintext}) {
          $fh->print ($data);
        } else {
          $fh->print (htescape $data);
        }
      } elsif ($nt == ELEMENT_NODE) {
        my $ns = $node->namespace_uri || '';
        my $ln = $node->manakai_local_name;
        my $attrs = [];
        if ($ns eq TEMMA_MSGID_NS) {
          next if $self->_close_start_tag ($process, $fh);
          $self->_before_non_space ($process => $fh);

          $self->_print_msgid ($node, $process => $ln => $fh);
          next;
        } elsif ($ns eq TEMMA_NS) {
          if ($ln eq 'text') {
            next if $self->_close_start_tag ($process, $fh);
            $self->_before_non_space ($process => $fh);
            
            my $value = $self->eval_attr_value
                ($node, 'value', disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info});
            if (defined $value) {
              if ($process->{node_info}->{rawtext}) {
                ${$process->{node_info}->{rawtext_value}} .= $value;
              } elsif ($process->{node_info}->{plaintext}) {
                $fh->print ($value);
              } else {
                $fh->print (htescape $value);
              }
            }
            next;
          } elsif ($ln eq 'element') {
            next if $self->_close_start_tag ($process, $fh);

            $ln = $self->eval_attr_value
                ($node, 'name', required => 'm',
                 node_info => $process->{node_info});
            if (defined $ln) {
              $ln =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
              $ns = $node->$TemmaContextNode->manakai_get_child_namespace_uri ($ln);
              if ($ns eq SVG_NS) {
                $ln = $Web::HTML::ParserData::SVGElementNameFixup->{$ln} || $ln;
              }
            }
          } elsif ($ln eq 'attr' or $ln eq 'class') {
            if ($self->{current_tag}) {
              next if $self->{current_tag}->{ln} eq '';

              my $attr_name = $ln eq 'class' ? 'class' :
                  $self->eval_attr_value ($node, 'name',
                                          node_info => $process->{node_info});
              $attr_name = '' unless defined $attr_name;
              $attr_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
              my $element_ns = $self->{current_tag}->{ns};
              if ($element_ns eq SVG_NS) {
                $attr_name = $Web::HTML::ParserData::SVGAttrNameFixup->{$attr_name} || $attr_name;
              } elsif ($element_ns eq MML_NS) {
                $attr_name = $Web::HTML::ParserData::MathMLAttrNameFixup->{$attr_name} || $attr_name;
              }
              ## $Web::HTML::ParserData::ForeignAttrNamespaceFixup
              ## is ignored here as the mapping is no-op for the
              ## purpose of qualified name serialization.

              unless ($attr_name =~ /\A[A-Za-z_-][A-Za-z0-9_:-]*\z/) {
                $self->onerror->(type => 'temma:name not serializable',
                                 node => $node,
                                 value => $attr_name,
                                 level => 'm');
                next;
              }

              if ($self->{current_tag}->{attrs}->{$attr_name}) {
                $self->onerror->(type => 'temma:duplicate attr',
                                 node => $node,
                                 value => $attr_name,
                                 level => 'm');
                next;
              }

              my $value = $self->eval_attr_value
                  ($node, $ln eq 'class' ? 'name' : 'value',
                   disallow_undef => 'w', required => 'm',
                   node_info => $process->{node_info});
              if (defined $value) {
                if ($attr_name eq 'class') {
                  push @{$self->{current_tag}->{classes} ||= []}, 
                      grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $value;
                } else {
                  $self->{current_tag}->{attrs}->{$attr_name} = 1;
                  $fh->print (' ' . $attr_name . '="' . (htescape $value) . '"');
                }
              }
            } else {
              $self->onerror->(type => 'temma:start tag already closed',
                               node => $node,
                               level => 'm');
            }
            next;
          } elsif ($ln eq 'comment') {
            next if $self->_close_start_tag ($process, $fh);

            if ($process->{node_info}->{rawtext} or
                $process->{node_info}->{plaintext}) {
              $self->onerror->(type => 'temma:comment not allowed',
                               node => $node,
                               level => 'm');
              next;
            }

            $self->_before_non_space ($process => $fh);

            my $node_info = {rawtext => 1, rawtext_value => \(my $v = ''),
                             allow_children => 1, comment => 1,
                             has_non_space => 1, preserve_space => 1,
                             binds => $process->{node_info}->{binds},
                             fields => $process->{node_info}->{fields}};

            unshift @{$self->{processes}},
                {type => 'end tag', node_info => $node_info};

            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} }
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node->child_nodes->to_a};

            $fh->print ("<!--");
            next;

          } elsif ($ln eq 'if') {
            $self->_before_non_space ($process => $fh, transparent => 1);

            my $value = $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'bool',
                 node_info => $process->{node_info});

            my $state = $value ? 'if-matched' : 'not yet';
            my @node;
            my $cond_node = $value ? $node : undef;
            for (@{$node->child_nodes->to_a}) {
              my $nt = $_->node_type;
              next unless $nt == ELEMENT_NODE or $nt == TEXT_NODE;
              my $ns = $_->namespace_uri || '';
              my $ln = $_->manakai_local_name || '';
              if ($ns eq TEMMA_NS and $ln eq 'elsif') {
                if ($state eq 'not yet') {
                  my $value = $self->eval_attr_value
                      ($_, 'x', required => 'm', context => 'bool',
                       node_info => $process->{node_info});
                  if ($value) {
                    $state = 'if-matched';
                    $cond_node = $_;
                  }
                } elsif ($state eq 'if-matched') {
                  last;
                } else {
                  $self->onerror->(type => 'element not allowed:t:if',
                                   level => 'm',
                                   node => $_);
                  last;
                }
              } elsif ($ns eq TEMMA_NS and $ln eq 'else') {
                if ($state eq 'if-matched') {
                  last;
                } elsif ($state eq 'not yet') {
                  $state = 'else-matched';
                  $cond_node = $_;
                } else {
                  $self->onerror->(type => 'element not allowed:t:if',
                                   level => 'm',
                                   node => $_);
                  last;
                }
              } else {
                if ($state =~ /matched/) {
                  push @node, $_;
                }
              }
            }
            next unless $cond_node;

            my $sp = _ascii_lc $cond_node->get_attribute_ns (TEMMA_NS, 'space') || '';
            $self->_schedule_nodes (\@node, $process->{node_info}, $sp);
            next;
          } elsif ($ln eq 'for') {
            $self->_before_non_space ($process => $fh, transparent => 1);

            my $items = $self->eval_attr_value
                ($node, 'x', required => 'm', disallow_undef => 'm',
                 node_info => $process->{node_info});
            $items = [] unless defined $items;
            my $item_count = do {
              local $@;
              eval { 0+@$items };
            };
            if (not defined $item_count) {
              $self->onerror->(type => 'temma:not arrayref',
                               node => $node->get_attribute_node ('x'),
                               level => 'm');
              $items = [];
              $item_count = 0;
            }

            if ($item_count > 0) {
              my $nodes = [];
              my $sep_nodes = [];
              my $sep_node;
              for my $node (@{$node->child_nodes->to_a}) {
                next unless $node->node_type == ELEMENT_NODE or
                            $node->node_type == TEXT_NODE;
                my $ns = $node->namespace_uri || '';
                my $ln = $node->manakai_local_name;
                if ($ns eq TEMMA_NS and $ln eq 'sep') {
                  if ($sep_node) {
                    $self->onerror->(type => 'temma:duplicate sep',
                                     node => $node,
                                     level => 'm');
                    last;
                  } else {
                    $sep_node = $node;
                  }
                } elsif ($sep_node) {
                  push @$sep_nodes, $node;
                } else {
                  push @$nodes, $node;
                } 
              }
              
              my $as = $node->get_attribute_ns (undef, 'as');
              if (defined $as) {
                $as =~ s/^\$//;
                if (not $as =~ /\A[A-Za-z_][0-9A-Za-z_]*\z/ or $as eq '_') {
                  $self->onerror->(type => 'temma:variable name',
                                   node => $node->get_attribute_node ('as'),
                                   level => 'm');
                  undef $as;
                }
              }
              
              my $block_name = $node->get_attribute ('name');
              $block_name = '' unless defined $block_name;
              unshift @{$self->{processes}},
                  {type => 'for block',
                   nodes => $nodes,
                   sep_nodes => $sep_nodes,
                   node_info => $process->{node_info},
                   space => _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '',
                   sep_space => $sep_node ? _ascii_lc $sep_node->get_attribute_ns (TEMMA_NS, 'space') || '' : undef,
                   items => $items,
                   index => 0,
                   bound_to => $as,
                   block_name => $block_name};
            }
            next;
          } elsif ($ln eq 'try') {
            $self->_before_non_space ($process => $fh, transparent => 1);
            
            my $nodes = [];
            my $catches = [];
            for my $node (@{$node->child_nodes->to_a}) {
              next unless $node->node_type == ELEMENT_NODE or
                          $node->node_type == TEXT_NODE;
              my $ns = $node->namespace_uri || '';
              my $ln = $node->manakai_local_name;
              if ($ns eq TEMMA_NS and $ln eq 'catch') {
                my $as = $node->get_attribute ('as');
                if (defined $as) {
                  $as =~ s/^\$//;
                  if (not $as =~ /\A[A-Za-z_][0-9A-Za-z_]*\z/ or
                      $as eq '_') {
                    $self->onerror->(type => 'temma:variable name',
                                     node => $node->get_attribute_node ('as'),
                                     level => 'm');
                    undef $as;
                  }
                }

                push @$catches,
                    {nodes => [],
                     package => $node->get_attribute ('package'),
                     sp => _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '',
                     bound_to => $as,
                     node_info => $process->{node_info}};
              } elsif (@$catches) {
                push @{$catches->[-1]->{nodes}}, $node;
              } else {
                push @$nodes, $node;
              } 
            }

            my $sp = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
            $self->_schedule_nodes
                ($nodes, $process->{node_info}, $sp, catches => $catches);
            next;
          } elsif ($ln eq 'my') {
            my $as = $node->get_attribute ('as');
            $as =~ s/^\$// if defined $as;
            if (not defined $as or
                not $as =~ /\A[A-Za-z_][0-9A-Za-z_]*\z/ or
                $as eq '_') {
              $self->onerror->(type => 'temma:variable name',
                               node => $node->get_attribute_node ('as') || $node,
                               level => 'm');
              next;
            }

            my $value = $self->eval_attr_value
                ($node, 'x', context => 'scalar',
                 node_info => $process->{node_info});
            $process->{node_info}->{binds}
                = {%{$process->{node_info}->{binds} || {}},
                   $as => [[$value], 0]};
            next;
          } elsif ($ln eq 'macro') {
            my $name = $node->get_attribute ('name');
            if (not defined $name) {
              $self->onerror->(type => 'attribute missing',
                               text => 'name',
                               level => 'm',
                               node => $node);
              next;
            } elsif (not $name =~ /\A[a-z_.][a-z_.0-9-]*\z/) {
              $self->onerror->(type => 'temma:bad macro name',
                               level => 'm',
                               node => $node);
              next;
            }

            if ($self->{macros}->{$name}) {
              $self->onerror->(type => 'temma:macro already defined',
                               level => 'm',
                               value => $name,
                               node => $node);
              next;
            }

            $self->{macros}->{$name}
                = {node => $node,
                   preserve_space => $process->{node_info}->{preserve_space},
                   params => $self->_get_params ($node)};

            next;
          } elsif ($ln eq 'content') {
            my $name = $node->get_attribute ('name');
            $name = '1' unless defined $name;

            my $def = $process->{node_info}->{fields}->{$name};
            next unless $def;

            $self->_schedule_nodes
              ([grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$def->{node}->child_nodes->to_a}],
               $process->{node_info}, $def->{sp}, binds => $def->{binds});

            next;
          } elsif ($ln eq 'include') {
            my $path = $node->get_attribute ('path');
            unless (defined $path) {
              $self->onerror->(type => 'attribute missing',
                               text => 'path',
                               level => 'm',
                               node => $node);
              next;
            }

            if (($process->{node_info}->{macro_depth} || 0) > 50) {
              $self->onerror->(type => 'temma:macro too deep',
                               level => 'm',
                               node => $node);
              next;
            }
            
            $self->_before_non_space ($process => $fh)
                unless $self->{current_tag};

            my $sp = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
            my ($fields, $has_field) = $self->_process_fields
                ($node, $sp, $process);

            my $parse_context = 'html';
            my $n = $node->parent_node;
            while ($n) {
              if ($n->node_type == ELEMENT_NODE and
                  ($n->manakai_local_name eq 'body' or
                   $n->manakai_local_name eq 'head')) {
                $parse_context = 'body';
                last;
              }
              $n = $n->parent_node;
            }

            my $x = {
              context => $node,
              path => $path,
              doc_to_path => $self->{doc_to_path},
              onerror => $self->onerror,
              create_document => sub {
                return $node->owner_document->implementation->create_document;
              },
              get_parser => sub {
                require Temma::Parser;
                my $parser = Temma::Parser->new;
                $parser->{initial_state} = $parse_context;
                $parser->di_data_set ($self->di_data_set);
                return $parser;
              },
            }; # $x
            my $onparsed = sub {
              my $html_el = $_[0]->manakai_html;
              my $binds = {has_field => $has_field};
              if ($html_el) {
                my $params = $self->_get_params ($html_el);
                for my $param (@{$params}) {
                  my $value = $self->eval_attr_value
                      ($node, $param->[0],
                       nsurl => TEMMA_MACRO_NS,
                       required => $param->[1] ? undef : 'm',
                       node_info => $process->{node_info});
                  $binds->{$param->[0]} = [[$value], 0];
                }
              }

              my $nodes;
              if ($parse_context eq 'html') {
                if ($html_el) {
                  my $attrs = $html_el->attributes;
                  if (@$attrs) {
                    if ($self->{current_tag} and
                        $self->{current_tag}->{lnn} eq 'html') {
                      $self->_print_attrs
                          ($attrs => $fh, $self->{current_tag});
                    } else {
                      $self->onerror->(type => 'temma:start tag already closed',
                                       node => $html_el,
                                       level => 'm');
                    }
                  }
                  $nodes = $html_el->child_nodes->to_a;
                }
              } else {
                my $body_el = $_[0]->body;
                $nodes = $body_el->child_nodes->to_a if $body_el;
              }
              $self->_schedule_nodes
                  ([grep { $_->node_type == ELEMENT_NODE or
                           $_->node_type == TEXT_NODE } @$nodes],
                   $process->{node_info}, 'trim',
                   binds => $binds,
                   fields => $fields,
                   is_entity_boundary => 1,
                   macro_depth => ($process->{node_info}->{macro_depth} || 0) + 1)
                      if $nodes;
              $self->_process ($fh);
            }; # onparsed
            my $onerror = sub {
              $self->onerror->(type => 'temma:include error',
                               level => 'm',
                               value => $_[0],
                               node => $node);
            }; # onerror

            my $code = $self->oninclude;
            my $result = eval { $code->($x) };
            if ($@) {
              $onerror->($@);
            } elsif (UNIVERSAL::can ($result, 'then')) {
              eval { $result->then ($onparsed, $onerror) };
              $onerror->($@) if $@;
            } else {
              $onparsed->($result);
            }
            return;
          } elsif ($ln eq 'call') {
            $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'void',
                 node_info => $process->{node_info});
            next;
          } elsif ($ln eq 'wait') {
            my $value = $self->eval_attr_value
                  ($node, 'cv', disallow_undef => 'm', required => 'm',
                   node_info => $process->{node_info});
            if (not defined $value) {
              #
            } elsif (not UNIVERSAL::can ($value, 'cb')) {
              $self->onerror->(type => 'temma:no cb method',
                               level => 'm',
                               node => $node->get_attribute_node ('cv'));
            } else {
              my $as = $node->get_attribute ('as');
              $as =~ s/^\$// if defined $as;
              if (not defined $as) {
                #
              } elsif (not $as =~ /\A[A-Za-z_][0-9A-Za-z_]*\z/ or $as eq '_') {
                $self->onerror->(type => 'temma:variable name',
                                 node => $node->get_attribute_node ('as') || $node,
                                 level => 'm');
                undef $as;
              }

              if ($fh->can ('autoflush')) {
                unless (my $af = $fh->autoflush) {
                  $fh->autoflush (1);
                  $fh->print ('');
                  $fh->autoflush ($af);
                }
              }

              eval {
                $value->cb (sub {
                  if ($as) {
                    $process->{node_info}->{binds}
                        = {%{$process->{node_info}->{binds} || {}},
                           $as => [[$_[0]->recv], 0]};
                  }

                  unshift @{$self->{processes}},
                      {type => 'eval_attr_value',
                       node => $node, attr_name => 'cb',
                       node_info => $process->{node_info}};
                  $self->_process ($fh);
                });
                1;
              } or do {
                $self->onerror->(type => 'temma:perl exception:cb',
                                 level => 'm',
                                 value => $@,
                                 node => $node->get_attribute_node ('cv'));
              };
              return;
            }
            next;
          } elsif ($ln eq 'last' or $ln eq 'next') {
            my $block_name = $node->get_attribute ('for');
            $block_name = '' unless defined $block_name;

            my $found;
            my $searched = $ln eq 'last' ? 'for block' : 'end block';
            my @close;
            while (@{$self->{processes}}) {
              my $process = shift @{$self->{processes}};
              if ($process->{type} eq $searched and
                  defined $process->{block_name} and
                  ($process->{block_name} eq $block_name or
                   $block_name eq '')) {
                $found = 1;
                last;
              } elsif ({
                end => 1, 'end tag' => 1, 'end block' => 1,
              }->{$process->{type}}) {
                last if $process->{is_entity_boundary};
                push @close, $process;
              }
            }

            if ($found) {
              unshift @{$self->{processes}}, @close;
            } else {
              $self->onerror->(type => 'temma:block not found',
                               value => $block_name,
                               node => $node,
                               level => 'm');
            }
            next;
          } elsif ($ln eq 'barehtml') {
            next if $self->_close_start_tag ($process, $fh);
            $self->_before_non_space ($process => $fh);
            
            my $value = $self->eval_attr_value
                ($node, 'value', disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info});
            if (defined $value) {
              if ($process->{node_info}->{rawtext} or
                  $process->{node_info}->{plaintext}) {
                $self->onerror->(type => 'element not allowed:rawtext',
                                 node => $node,
                                 level => 'm');
              } else {
                $fh->print ($value);
              }
            }
            next;
          } else { # $ln
            next if $self->_close_start_tag ($process, $fh);

            if ($ln eq 'else' or $ln eq 'elsif') {
              $self->onerror->(type => 'element not allowed',
                               node => $node,
                               level => 'm');
            } else { ## Unknown element in Temma namespace
              $self->onerror->(type => 'temma:unknown element',
                               node => $node,
                               value => $ln,
                               level => 'm');
            }
            next;
          } # $ln
        } elsif ($ns eq TEMMA_MACRO_NS) {
          my $ln = $node->manakai_local_name;
          my $macro = $self->{macros}->{$ln};
          unless ($macro) {
            $self->onerror->(type => 'temma:macro not defined',
                             level => 'm',
                             value => $ln,
                             node => $node);
            next;
          }

          if (($process->{node_info}->{macro_depth} || 0) > 50) {
            $self->onerror->(type => 'temma:macro too deep',
                             level => 'm',
                             node => $node);
            next;
          }

          my $sp = _ascii_lc $macro->{node}->get_attribute_ns (TEMMA_NS, 'space') || '';
          my ($fields, $has_field) = $self->_process_fields
              ($node, $sp, $process);

          my $binds = {has_field => $has_field};
          for my $param (@{$macro->{params}}) {
            my $value = $self->eval_attr_value
                ($node, $param->[0],
                 nsurl => TEMMA_MACRO_NS,
                 required => $param->[1] ? undef : 'm',
                 node_info => $process->{node_info});
            $binds->{$param->[0]} = [[$value], 0];
          }
          
          $sp = {preserve => 'preserve', trim => 'trim'}->{$sp} ||
              ($macro->{preserve_space} ? 'preserve' : 'trim');
          $self->_schedule_nodes
              ([grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$macro->{node}->child_nodes->to_a}],
               $process->{node_info}, $sp,
               binds => $binds,
               fields => $fields,
               is_entity_boundary => 1,
               macro_depth => ($process->{node_info}->{macro_depth} || 0) + 1);

          next;
        } else { # $ns
          next if $self->_close_start_tag ($process, $fh);
          $attrs = $node->attributes;
        } # $ns

        if (not $process->{node_info}->{allow_children} or
            $process->{node_info}->{rawtext} or
            $process->{node_info}->{plaintext}) {
          unless ($process->{node_info}->{children_not_allowed_error}) {
            $self->onerror->(type => 'temma:child not allowed',
                             node => $node,
                             level => 'm');
            $process->{node_info}->{children_not_allowed_error} = 1;
          }
          next;
        }

        if (($self->{need_head_end_tag} || 0) > 0) {
          if (defined $ln and $ln eq 'head' and not @$attrs) {
            undef $ln;
          } else {
            $fh->print ('</head>');
          }
        }

        $self->_before_non_space ($process => $fh);

        my $node_info = {node => $node, attrs => {},
                         binds => $process->{node_info}->{binds},
                         fields => $process->{node_info}->{fields},
                         macro_depth => $process->{node_info}->{macro_depth}};

        if (defined $ln and $ln =~ /\A[A-Za-z_-][A-Za-z0-9_-]*\z/) {
          $fh->print ('<' . $ln);
          $self->{current_tag} = $node_info;
          $node_info->{ns} = $ns;
          $node_info->{ln} = $ln;
          $node_info->{lnn} = $ln;
          $node_info->{lnn} =~ tr/A-Z/a-z/; ## ASCII case-insensitive.

          if ($node_info->{ns} eq HTML_NS and
              $Web::HTML::ParserData::AllVoidElements->{$node_info->{lnn}}) {
            unshift @{$self->{processes}}, {type => 'end'};

            $self->_print_attrs ($attrs => $fh, $node_info);

            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} } 
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node_info->{node}->child_nodes->to_a};
          } else {
            $node_info->{allow_children} = 1;
            if ($node_info->{ns} eq HTML_NS) {
              if ($node_info->{lnn} eq 'script' or
                  $node_info->{lnn} eq 'style') {
                $node_info->{rawtext} = 1;
                $node_info->{rawtext_value} = \(my $v = '');
              } elsif ($node_info->{lnn} eq 'html') {
                $self->_get_params
                    ($node_info->{node},
                     bind_args => $node_info->{binds} ||= {});
              }
            }

            my $sp = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
            if ($sp eq 'preserve' or
                (($Temma::Defs::PreserveWhiteSpace->{$node_info->{ns}}->{$node_info->{ln}} or
                  $process->{node_info}->{preserve_space}) and
                 not ($sp eq 'trim'))) {
              $node_info->{preserve_space} = 1; # for descendants
              $node_info->{has_non_space} = 1; # for children
            }

            unshift @{$self->{processes}},
                {type => 'end tag', node_info => $node_info};
            
            $self->_print_attrs ($attrs => $fh, $node_info);

            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} } 
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{((($node_info->{node}->namespace_uri || '') eq HTML_NS and
                    $node_info->{node}->local_name eq 'template')
                       ? $node_info->{node}->content : $node_info->{node})
                      ->child_nodes->to_a};
          }
        } else {
          if (defined $ln) {
            ## The element is not in the temma namespace and its local
            ## name is not serializable.  The element is ignored but
            ## its content is processed.

            $self->onerror->(type => 'temma:name not serializable',
                             node => $node,
                             value => $ln,
                             level => 'm');
          }

          $node_info->{allow_children} = 1;
          my $sp = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
          if ($sp eq 'preserve' or
              ($process->{node_info}->{preserve_space} and
               not $sp eq 'trim')) {
            $node_info->{preserve_space} = 1; # for descendants
            $node_info->{has_non_space} = 1; # for children
          }
          $self->{current_tag} = {ln => '', lnn => ''};

          if (($self->{need_head_end_tag} || 0) > 0) {
            unshift @{$self->{processes}},
                {type => 'end tag', node_info => {ln => 'head'}};
          }

          unshift @{$self->{processes}}, {type => 'end'};
          unshift @{$self->{processes}},
              map { {type => 'node', node => $_, node_info => $node_info} } 
              grep { $_->node_type == ELEMENT_NODE or
                     $_->node_type == TEXT_NODE }
              @{$node->child_nodes->to_a};
        }
        $self->{need_head_end_tag}-- if $self->{need_head_end_tag};
      } elsif ($nt == DOCUMENT_TYPE_NODE) {
        next if $self->_close_start_tag ($process, $fh);

        my $nn = $node->node_name;
        $nn =~ s/[^0-9A-Za-z_-]/_/g;
        $fh->print ('<!DOCTYPE ' . $nn . '>');
      } elsif ($nt == DOCUMENT_NODE) {
        my $node_info = {allow_children => 1};
        unshift @{$self->{processes}},
            map { {type => 'node', node => $_, node_info => $node_info} } 
            grep { $_->node_type == ELEMENT_NODE or
                   $_->node_type == DOCUMENT_TYPE_NODE }
            @{$node->child_nodes->to_a};
      } else {
        die "Unknown node type |$nt|";
      }

    } elsif ($process->{type} eq 'end tag') {
      next if $self->_close_start_tag ($process, $fh);

      if ($process->{node_info}->{comment}) {
        my $value = ${$process->{node_info}->{rawtext_value}};
        $value =~ s/--/- - /g;
        $value =~ s/-\z/- /;
        $fh->print ($value, "-->");
        next;
      } elsif ($process->{node_info}->{rawtext}) {
        my $value = ${$process->{node_info}->{rawtext_value}};

        my $dom = $process->{node_info}->{node}->owner_document->implementation;
        my $doc = $dom->create_document;
        $doc->manakai_is_html (1);

        my $el = $doc->create_element ('div');
        $el->inner_html ('<' . $process->{node_info}->{ln} . '>' .
                         $value .
                         '</' . $process->{node_info}->{ln} . '>');
        my $value2 = $el->first_child->text_content;
        if ($value ne $value2) {
          $self->onerror->(type => 'temma:not representable in raw text',
                           node => $process->{node_info}->{node},
                           level => 'm');
        }
        
        $fh->print ($value2);
      }

      if ($process->{node_info}->{ln} eq 'head') {
        $self->{need_head_end_tag}++;
      } else {
        while (($self->{need_head_end_tag} || 0) > 0) {
          $fh->print ('</head>');
          $self->{need_head_end_tag}--;
        }
        $fh->print ('</' . $process->{node_info}->{ln} . '>');
      }
    } elsif ($process->{type} eq 'text') {
      if ($process->{node_info}->{rawtext}) {
        ${$process->{node_info}->{rawtext_value}} .= $process->{value};
      } elsif ($process->{node_info}->{plaintext}) {
        $fh->print ($process->{value});
      } else {
        $fh->print (htescape $process->{value});
      }
    } elsif ($process->{type} eq 'for block') {
      my $index = $process->{index};
      next unless defined $index; # end of for
      my $block_name = $process->{block_name};
      if (++$process->{index} <= $#{$process->{items}}) {
        unshift @{$self->{processes}}, $process;
        $self->_schedule_nodes
            ($process->{sep_nodes}, $process->{node_info},
             {preserve => 'preserve', trim => 'trim'}->{$process->{sep_space}} || $process->{space},
             binds => $process->{node_info}->{binds})
                if @{$process->{sep_nodes}};
      } else {
        unshift @{$self->{processes}},
            {type => 'for block',
             block_name => $block_name}; # end of for
      }
      
      my $binds = $process->{node_info}->{binds} || {};
      if ($process->{bound_to}) {
        $binds = {%$binds, $process->{bound_to} => [$process->{items}, $index]};
      }
      $self->_schedule_nodes
          ($process->{nodes}, $process->{node_info}, $process->{space},
           binds => $binds, block_name => $block_name);
    } elsif ($process->{type} eq 'end block') {
      $process->{parent_node_info}->{has_non_space} = 1
          if $process->{node_info}->{has_non_space};
    } elsif ($process->{type} eq 'eval_attr_value') {
      $self->eval_attr_value ($process->{node}, $process->{attr_name},
                              node_info => $process->{node_info});
    } elsif ($process->{type} eq 'end') {
      next if $self->_close_start_tag ($process, $fh);
      $fh->print ('');
      if ($process->{ondone}) {
        $self->_cleanup;
        $process->{ondone}->($self);
      }
    } elsif ($process->{type} eq 'barehtml') {
      if ($process->{node_info}->{rawtext} or
          $process->{node_info}->{plaintext}) {
        $self->onerror->(type => 'element not allowed:rawtext',
                         node => $process->{node},
                         level => 'm');
      } else {
        $fh->print ($process->{value});
      }
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  } # @{$self->{processes}}
} # __process

## Schedule processing of nodes, used for processing of <t:if>,
## <t:for>, and <t:try>.
sub _schedule_nodes ($$$$;%) {
  my ($self, $nodes, $parent_node_info, $sp, %args) = @_;

  my $node_info = {
    %{$parent_node_info},
    trailing_space => '',
    binds => $args{binds} || $parent_node_info->{binds},
    fields => $args{fields} || $parent_node_info->{fields},
    macro_depth => $args{macro_depth} || $parent_node_info->{macro_depth},
  };
  $node_info->{preserve_space}
      = $sp eq 'preserve' ? 1 :
        $sp eq 'trim' ? 0 :
        $parent_node_info->{preserve_space};
  $node_info->{has_non_space} = $node_info->{preserve_space};
  
  unshift @{$self->{processes}},
      {type => 'end block', node_info => $node_info,
       parent_node_info => $parent_node_info,
       block_name => $args{block_name},
       catches => $args{catches},
       is_entity_boundary => $args{is_entity_boundary}};
  unshift @{$self->{processes}},
      map { {type => 'node', node => $_, node_info => $node_info} } @$nodes;
} # _schedule_nodes

sub _print_attrs ($$$$) {
  my ($self, $attrs => $fh, $node_info) = @_;
  my @attr;
  for my $attr (@$attrs) {
    ## Note that if there are multiple attributes with only case
    ## differences in their names, or with same qualified name but in
    ## different namespaces, then, in HTML serialization, the only
    ## first one has effect (and the name is interpreted ASCII
    ## case-insensitively by parser).  We don't try to detect such an
    ## error as it is unlikely happens in typical use cases.

    my $attr_ns = $attr->namespace_uri;
    my $attr_name;
    my $value;
    if (not defined $attr_ns) {
      $attr_name = $attr->manakai_local_name;
      $value = $attr->value;
    } elsif ($attr_ns eq XML_NS) {
      $attr_name = 'xml:' . $attr->manakai_local_name;
      $value = $attr->value;
    } elsif ($attr_ns eq XMLNS_NS) {
      $attr_name = 'xmlns:' . $attr->manakai_local_name;
      $attr_name = 'xmlns' if $attr_name eq 'xmlns:xmlns';
      $value = $attr->value;
    } elsif ($attr_ns eq XLINK_NS) {
      $attr_name = 'xlink:' . $attr->manakai_local_name;
      $value = $attr->value;
    } elsif ($attr_ns eq TEMMA_MSGID_NS) {
      my $msgid = $attr->value;
      my $locale = $self->{locale} || do {
        $self->onerror->(type => 'temma:no locale',
                         node => $attr,
                         level => 'm');
        undef;
      };
      $value = $locale && $locale->plain_text ($msgid);
      $value = $msgid if not defined $value;
      $attr_name = $attr->manakai_local_name;
    } elsif ($attr_ns eq TEMMA_PERL_NS) {
      $value = $self->eval_attr_value
          ($attr->owner_element, $attr->node_name,
           attr_node => $attr,
           node_info => $node_info);
      next unless defined $value;
      $attr_name = $attr->manakai_local_name;
    } elsif ($attr_ns eq TEMMA_NS) {
      next;
    } else {
      $attr_name = $attr->name;
      $value = $attr->value;
    }

    unless ($attr_name =~ /\A[A-Za-z_-][A-Za-z0-9_:-]*\z/) {
      $self->onerror->(type => 'temma:name not serializable',
                       node => $attr,
                       value => $attr_name,
                       level => 'm');
      next;
    }

    if ($attr_name eq 'class') {
      push @{$node_info->{classes} ||= []},
          grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $value;
    } else {
      if ($node_info->{attrs}->{$attr_name}) {
        $self->onerror->(type => 'temma:duplicate attr',
                         node => $attr,
                         value => $attr_name,
                         level => 'm');
        next;
      }
      $node_info->{attrs}->{$attr_name} = 1;
      push @attr, ' ' . $attr_name . '="' . (htescape $value) . '"';
    }
  }
  $fh->print (join '', @attr) if @attr;
} # _print_attrs

sub _close_start_tag ($$$) {
  my ($self, $current_process, $fh) = @_;
  return 0 unless my $node_info = delete $self->{current_tag};
  return 0 if $node_info->{ln} eq '';
  
  if (@{$node_info->{classes} or []}) {
    $fh->print (q< class=">);
    $fh->print (htescape join ' ', @{$node_info->{classes}});
    $fh->print (q<">);
  }
  $fh->print ('>');
  unshift @{$self->{processes}}, $current_process;

  if ($Temma::Defs::IgnoreFirstNewline->{$node_info->{ln}} and
      $node_info->{ns} eq HTML_NS) {
    $fh->print ("\x0A");
  }

  return 1;
} # _close_start_tag

sub _before_non_space ($$;%) {
  my ($self, $process => $fh, %args) = @_;
  if (defined $process->{node_info}->{trailing_space}) {
    if ($process->{node_info}->{has_non_space}) {
      if ($process->{node_info}->{rawtext}) {
        ${$process->{node_info}->{rawtext_value}}
            .= $process->{node_info}->{trailing_space};
      } elsif ($process->{node_info}->{plaintext}) {
        $fh->print ($process->{node_info}->{trailing_space});
      } else {
        $fh->print (htescape $process->{node_info}->{trailing_space});
      }
    }
    delete $process->{node_info}->{trailing_space};
  }
  $process->{node_info}->{has_non_space} = 1 unless $args{transparent};
} # _before_non_space

sub eval_attr_value ($$$;%) {
  my ($self, $node, $name, %args) = @_;
  
  my $attr_node = $args{attr_node} ||
      $node->get_attribute_node_ns ($args{nsurl}, $name) or do {
    if ($args{required}) {
      $self->onerror->(type => 'attribute missing',
                       text => ($args{nsurl} && $args{nsurl} eq TEMMA_MACRO_NS ? 'm:' . $name : $name),
                       level => $args{required},
                       node => $node);
    }
    return undef;
  };

  my $parent = $node;
  my $locations = $self->{location_cache};
  my @descendant;
  while ($parent) {
    if ($locations->{$parent}) {
      unshift @{$locations->{$_}}, @{$locations->{$parent}} for @descendant;
      last;
    } else {
      if ($parent->node_type == ELEMENT_NODE) {
        my $nn = $parent->manakai_tag_name || $parent->node_name;
        unshift @{$locations->{$_}}, $nn for @descendant;
        $locations->{$parent} = [$nn];
        push @descendant, $parent;
      }
      $parent = $parent->parent_node;
    }
  }
  my $location = (join '>', @{$locations->{$node}}) . '[' . $name . ']';
  my ($di, $index);
  for ($attr_node, $node) {
    my $sl = $_->manakai_get_source_location;
    unless ($sl->[1] == -1) {
      $di = $sl->[1];
      $index = $sl->[2];
      last;
    }
  }
  if (defined $di) {
    my $dids = $self->di_data_set;
    my ($line, $column) = index_pair_to_lc_pair $dids, $di, $index;
    my $pa = $self->{doc_to_path}->{$node->owner_document || $node};
    $location .= sprintf ' (at %sline %d column %d)',
        defined $pa ? $pa . ' ' : '', $line || 0, $column || 0;
  }
  $location =~ s/[\x00-\x1F\x22]+/ /g;
  my $value = qq<local \$_;\n#line 1 "$location"\n> . $attr_node->value . q<;>;

  local $_ = $args{node_info}->{binds} || {};
  if (keys %$_) {
    $value = "return do {\n$value\n};";
    for my $var (keys %$_) {
      if (ref $_->{$var}->[0] eq 'HASH') {
        $value = qq{for my \$$var (\$_->{$var}->[0]->{$_->{$var}->[1]}) {\n$value\n}\n};
      } else {
        $value = qq{for my \$$var (\$_->{$var}->[0]->[$_->{$var}->[1]]) {\n$value\n}\n};
      }
    }
    $value = qq{#line 1 "vars for $location"\n} . $value;
  }
  $self->{eval_package} ||= qq{Temma::Eval::@{[time]}::@{[int rand 100000]}};
  $value = qq{package $self->{eval_package};\n$value};

  my $evaled;
  my $error;
  my $context = $args{context} || '';
  {
    local $@;
    if ($context eq 'bool') {
      $evaled = !! (_eval $value);
    } elsif ($context eq 'list') {
      $evaled = [_eval $value];
    } elsif ($context eq 'void') {
      _eval $value;
    } else {
      $evaled = _eval $value;
    }
    $error = $@;
  }
  if ($error) {
    require Temma::Exception;
    my $exception = Temma::Exception->new_from_value ($error);
    $exception->source_text ($value);
    $exception->source_node ($attr_node);
    die $exception;
  }

  if ($args{disallow_undef} and not defined $evaled) {
    $self->onerror->(type => 'temma:undef',
                     level => $args{disallow_undef},
                     node => $attr_node);
  }

  return $evaled;
} # eval_attr_value

sub _get_params ($$;%) {
  my ($self, $node, %args) = @_;
  my $params = $node->get_attribute_ns (TEMMA_NS, 'params');
  my $param_found = {};
  $params = defined $params
      ? [grep {
           if ($param_found->{$_->[0]}) {
             $self->onerror->(type => 'temma:duplicate param',
                              value => $_->[0],
                              node => $node->get_attribute_node_ns (TEMMA_NS, 'params'),
                              level => 'm');
             0;
           } else {
             $param_found->{$_->[0]} = 1;
             1;
           }
         } map {
           s/^\$//; s/\?$// ? [$_, 1] : [$_, 0];
         } grep {
           if (/\A\$[A-Za-z_][A-Za-z0-9_]*\??\z/) {
             1;
           } else {
             if (length) {
               $self->onerror->(type => 'temma:bad params',
                                value => $_,
                                node => $node->get_attribute_node_ns(TEMMA_NS, 'params') || $node,
                                level => 'm');
             }
             0;
           }
         } split /[\x09\x0A\x0C\x0D\x20]+/, $params]
      : [];

  if (my $binds = $args{bind_args}) {
    my $vars = $self->{args} ||= {};
    for my $param (@{$params}) {
      if (not $param->[1] and not exists $vars->{$param->[0]}) {
        $self->onerror->(type => 'temma:no param',
                         value => $param->[0],
                         node => $node->get_attribute_node_ns (TEMMA_NS, 'params'),
                         level => 'm');
      }
      $binds->{$param->[0]} = [$vars, $param->[0]];
    }
  }

  return $params;
} # _get_params

sub _process_fields ($$$$) {
  my ($self, $node, $sp, $process) = @_;
  
  my $fields = {};
  my $anon_id = 1;
  for (@{$node->child_nodes->to_a}) {
    next unless $_->node_type == ELEMENT_NODE;
    next unless ($_->namespace_uri || '') eq TEMMA_NS;
    next unless $_->manakai_local_name eq 'field';

    my $name = $_->get_attribute ('name');
    $name = $anon_id++ unless defined $name;
    if ($fields->{$name}) {
      $self->onerror->(type => 'temma:duplicate field',
                       value => $name,
                       level => 'm',
                       node => $_);
      next;
    }

    my $_sp = _ascii_lc $_->get_attribute_ns (TEMMA_NS, 'space') || '';
    my $sp = {preserve => 'preserve', trim => 'trim'}->{$_sp}
        || {preserve => 'preserve', trim => 'trim'}->{$sp}
        || ($process->{node_info}->{preserve_space} ? 'preserve' : 'trim');
    $fields->{$name} = {node => $_, sp => $sp,
                        binds => $process->{node_info}->{binds}};
  }
  my $has_field = [[{map { $_ => 1 } keys %$fields}], 0];
  
  return ($fields, $has_field);
} # _process_fields

sub _print_msgid ($$$$$;%) {
  my ($self, $node, $process => $msgid, $fh, %args) = @_;

  my $n = $self->eval_attr_value
      ($node, 'n', node_info => $process->{node_info});

  my $locale = $self->{locale} || do {
    $self->onerror->(type => 'temma:no locale',
                     node => $node,
                     level => 'm');
    undef;
  };
  my $set = $node->get_attribute ('set');
  $locale = $locale->for_text_set ($set) if defined $set;

  my $barehtml = $node->has_attribute ('barehtml');
  my $method = ($barehtml ? 'html' : 'plain_text') .
               (defined $n ? '_n' : '') .
               '_as_components';
  my $texts = $locale && $locale->$method ($msgid, defined $n ? (0+$n) : ());

  if ($texts and ref $texts eq 'ARRAY') {
    if (@$texts == 1 and $texts->[0]->{type} eq 'text') {
      if ($process->{node_info}->{rawtext}) {
        #
      } elsif ($process->{node_info}->{plaintext}) {
        $fh->print ($texts->[0]->{value});
        return;
      } else {
        $fh->print (htescape $texts->[0]->{value});
        return;
      }
    }

    my $sp = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
    my ($fields, $has_field) = $self->_process_fields ($node, $sp, $process);
    
    my $binds = {%{$process->{node_info}->{binds}}, 'n' => [[$n], 0]};

    for (reverse @$texts) {
      if ($_->{type} eq 'text') {
        unshift @{$self->{processes}},
            {type => 'text', value => $_->{value},
             node_info => $process->{node_info}};
      } elsif ($_->{type} eq 'field') {
        my $def = $fields->{$_->{name}};
        if ($def) {
          $self->_schedule_nodes
              ([grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$def->{node}->child_nodes->to_a}],
               $process->{node_info}, $def->{sp},
               binds => $binds);
        }
      } elsif ($_->{type} eq 'html' and $barehtml) {
        unshift @{$self->{processes}},
            {type => 'barehtml', value => $_->{value},
             node => $node, node_info => $process->{node_info}};
      } else { # $_->{type} unknown
        $self->onerror->(type => 'temma:components:unknown type',
                         value => $_->{type},
                         level => 'm',
                         node => $node);
      } # $_->{type}
    }
  } else {
    my $msgstr = $node->get_attribute ('alt');
    unshift @{$self->{processes}},
        {type => 'text',
         value => defined $msgstr ? $msgstr : $msgid,
         node_info => $process->{node_info}};
  }
} # _print_msgid

sub _cleanup ($) {
  my $self = $_[0];
  if (defined $self->{eval_package}) {
    no strict;
    %{$self->{eval_package}.'::'} = ();
  }
} # _cleanup

1;

=head1 LICENSE

Copyright 2012-2016 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
