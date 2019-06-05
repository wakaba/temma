package Temma::Compiler;
use strict;
use warnings;
no warnings 'utf8';
#
sub _eval ($) {
  return eval ('local @_;' . "\n" . $_[0]);
} # _eval
#
our $VERSION = '5.0';
use Symbol qw(delete_package);
use Web::DOM::Document;
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

    my $doc = Web::DOM::Document->new;
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

# XXX
use Promised::Flow;
sub compile ($$) {
  my ($self, $doc) = @_;

  $self->{location_cache} = {};

  #XXX
  my $f = $doc->get_user_data ('manakai_source_f');
  if (UNIVERSAL::isa ($f, 'Path::Class::File')) {
    $self->{doc_to_path}->{$doc} = $f;
  }

  return $self->_compile ([$doc]);
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

#      next if $self->_close_start_tag ($process);
#      $self->_cleanup;
#      $process->{ondone}->($self->{result});

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

# XXX t:macro is not allowed within t:macro, t:param, t:if, t:for

sub _compile ($) {
  my $self = shift;

  my $result = [];
  my $processes = [map {
      if ($_->node_type == 3) {
        {type => 'text', value => $_->node_value};
      } else { # 1, document
        {type => 'node', node => $_};
      }
    } @{$_[0]}];

  return Promise->resolve->then (sub {

  return promised_until {
    return 'done' unless @$processes;

  while (@{$processes}) {
    my $process = shift @{$processes};

    if ($process->{type} eq 'node') {
      my $node = $process->{node};
      my $nt = $node->node_type;
      if ($nt == TEXT_NODE) {
        my $data = $node->data;
        if ($data =~ /[^\x09\x0A\x0C\x0D\x20]/) {
          ## Non white-space node
          next if $self->_close_start_tag ($process => $processes => $result);
          if (not $process->{node_info}->{has_non_space} and 
              not $process->{node_info}->{preserve_space}) {
            $data =~ s/^[\x09\x0A\x0C\x0D\x20]+//;
            $process->{node_info}->{has_non_space} = 1;
          }
        } else {
          ## White space only Text node
          if ($process->{node_info}->{preserve_space}) {
            next if $self->_close_start_tag ($process => $processes => $result);
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
          push @$result, ['rawtext', $data];
        } elsif ($process->{node_info}->{plaintext}) {
          push @$result, $data;
        } else {
          push @$result, htescape $data;
        }
      } elsif ($nt == ELEMENT_NODE) {
        my $ns = $node->namespace_uri || '';
        my $ln = $node->manakai_local_name;
        my $attrs = [];
        if ($ns eq TEMMA_NS) {
          if ($ln eq 'text') { # <t:text>
            next if $self->_close_start_tag ($process => $processes => $result);
            $self->_before_non_space ($process => $result);

            my $dest;
            if ($process->{node_info}->{rawtext}) {
              $dest = 'rawtext';
              $process->{node_info}->{rawtext_has_eval} = 1;
            } elsif ($process->{node_info}->{plaintext}) {
              $dest = 'print';
            } else {
              $dest = 'printescaped';
            }
            push @$result, $self->eval_attr_value
                ($node, 'value', disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info},
                 destination => $dest);
            next;
          } elsif ($ln eq 'element') { # <t:element>
            next if $self->_close_start_tag ($process => $processes => $result);

            push @$result, $self->eval_attr_value
                ($node, 'name', required => 'm',
                 node_info => $process->{node_info},
                 destination => 'openstart');

            # XXX children
            next;
          } elsif ($ln eq 'class') { # <t:class>
            unless (defined $self->{current_tag}) {
              $self->onerror->(type => 'temma:start tag already closed',
                               node => $node,
                               level => 'm');
              next;
            }

            push @$result, $self->eval_attr_value
                ($node, 'name',
                 disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info},
                 destination => 'classes');
            next;
          } elsif ($ln eq 'attr') { # <t:attr>
            unless (defined $self->{current_tag}) {
              $self->onerror->(type => 'temma:start tag already closed',
                               node => $node,
                               level => 'm');
              next;
            }

            push @$result, $self->eval_attr_value
                ($node, 'name', node_info => $process->{node_info},
                 destination => 'attr_name');
            push @$result, $self->eval_attr_value
                ($node, 'value',
                 disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info},
                 destination => 'attr');
            next;
          } elsif ($ln eq 'comment') { # <t:comment>
            next if $self->_close_start_tag ($process => $processes => $result);

            if ($process->{node_info}->{rawtext} or
                $process->{node_info}->{plaintext}) {
              $self->onerror->(type => 'temma:comment not allowed',
                               node => $node,
                               level => 'm');
              next;
            }

            $self->_before_non_space ($process => $result);

            my $node_info = {rawtext => 1,
                             allow_children => 1, comment => 1,
                             has_non_space => 1, preserve_space => 1,
                             binds => $process->{node_info}->{binds},
                             fields => $process->{node_info}->{fields}};

            unshift @{$processes},
                {type => 'end tag', node_info => $node_info};

            unshift @{$processes},
                map { {type => 'node', node => $_, node_info => $node_info} }
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node->child_nodes->to_a};

            push @$result, "<!--";
            next;

          } elsif ($ln eq 'if') { # <t:if>
            $self->_before_non_space ($process => $result, transparent => 1);

            push @$result, my $if = ['if'];
            
            push @$if, my $ww = [undef, undef, []];
            $ww->[0] = $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'bool',
                 node_info => $process->{node_info});
            $ww->[1] = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';

            my $has_else;
            for (@{$node->child_nodes->to_a}) {
              my $nt = $_->node_type;
              next unless $nt == ELEMENT_NODE or $nt == TEXT_NODE;
              my $ns = $_->namespace_uri || '';
              my $ln = $_->manakai_local_name || '';
              if ($ns eq TEMMA_NS and $ln eq 'elsif') {
                if ($has_else) {
                  $self->onerror->(type => 'element not allowed:t:if',
                                   level => 'm',
                                   node => $_);
                  last;
                }

                push @$if, $ww = [undef, undef, []];
                $ww->[0] = $self->eval_attr_value
                    ($_, 'x', required => 'm', context => 'bool',
                     node_info => $process->{node_info});
                $ww->[1] = _ascii_lc $_->get_attribute_ns (TEMMA_NS, 'space') || '';
              } elsif ($ns eq TEMMA_NS and $ln eq 'else') {
                if ($has_else) {
                  $self->onerror->(type => 'element not allowed:t:if',
                                   level => 'm',
                                   node => $_);
                  last;
                }
                $has_else = 1;

                push @$if, $ww = [1, undef, []];
                $ww->[1] = _ascii_lc $_->get_attribute_ns (TEMMA_NS, 'space') || '';
              } else {
                push @{$ww->[2]}, $_;
              }
            } # child node

            return Promise->resolve->then (sub {
              return promised_for {
                my $v = $_[0];
                return $self->_compile ($v->[2])->then (sub {
                  $v->[2] = $_[0];
                });
              } [@$if[1..$#$if]];
            })->then (sub {
              return not 'done';
            });
          } elsif ($ln eq 'for') { # <t:for>
            $self->_before_non_space ($process => $result, transparent => 1);

            push @$result,
                my $for = ['for', undef, undef, undef, undef, [], '', []];

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
            $for->[1] = $as;

            my $block_name = $node->get_attribute ('name');
            $block_name = '' unless defined $block_name;
            $for->[2] = $block_name;
            
            $for->[3] = $self->eval_attr_value
                ($node, 'x', required => 'm', disallow_undef => 'm',
                 node_info => $process->{node_info});
            $for->[4] = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';

            my $container = $for->[5];
            my $has_sep;
            for my $node (@{$node->child_nodes->to_a}) {
              next unless $node->node_type == ELEMENT_NODE or
                          $node->node_type == TEXT_NODE;
              my $ns = $node->namespace_uri || '';
              my $ln = $node->manakai_local_name;
              if ($ns eq TEMMA_NS and $ln eq 'sep') {
                if ($has_sep) {
                  $self->onerror->(type => 'temma:duplicate sep',
                                   node => $node,
                                   level => 'm');
                  last;
                }
                $has_sep = 1;
                $for->[6] = _ascii_lc $node->get_attribute_ns (TEMMA_NS, 'space') || '';
                $container = $for->[7];
              } else {
                push @$container, $node;
              }
            } # child nodes

            return $self->_compile ($for->[5])->then (sub {
              $for->[5] = $_[0];
              return $self->_compile ($for->[7]);
            })->then (sub {
              $for->[7] = $_[0];
              return not 'done';
            });
          } elsif ($ln eq 'my') { # <t:my>
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

            my $vx = $self->eval_attr_value
                ($node, 'x', context => 'scalar',
                 node_info => $process->{node_info});
            $process->{node_info}->{binds}
                = {%{$process->{node_info}->{binds} || {}}};
            $process->{node_info}->{binds}->{$as} = [my $x = [], 0];
            push @$result, ['my', $vx => $x, 0];
            next;
          } elsif ($ln eq 'macro') { # <t:macro>
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
            # XXX output
            
            next;
          } elsif ($ln eq 'content') { # <t:content>
            my $name = $node->get_attribute ('name');
            $name = '1' unless defined $name;
            push @$result, ['content', $name];
            next;
          } elsif ($ln eq 'include') { # <t:include>
            my $path = $node->get_attribute ('path');
            unless (defined $path) {
              $self->onerror->(type => 'attribute missing',
                               text => 'path',
                               level => 'm',
                               node => $node);
              next;
            }

            #XXX
            if (($process->{node_info}->{macro_depth} || 0) > 50) {
              $self->onerror->(type => 'temma:macro too deep',
                               level => 'm',
                               node => $node);
              next;
            }
            
            $self->_before_non_space ($process => $result) unless $self->{current_tag};

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

            push @$result, ['include', $path];
          } elsif ($ln eq 'call') { # <t:call>
            push @$result, $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'void',
                 node_info => $process->{node_info},
                 destination => 'nop');
            next;
          } elsif ($ln eq 'last' or $ln eq 'next') { # <t:last> <t:next>
            my $block_name = $node->get_attribute ('for');
            $block_name = '' unless defined $block_name;
            # XXX compile-time error if no block
            push @$result, [$ln, $block_name];
            next;
          } elsif ($ln eq 'barehtml') { # <t:barehtml>
            next if $self->_close_start_tag ($process => $processes => $result);
            $self->_before_non_space ($process => $result);
            
            if ($process->{node_info}->{rawtext} or
                $process->{node_info}->{plaintext}) {
              $self->onerror->(type => 'element not allowed:rawtext',
                               node => $node,
                               level => 'm');
            } else {
              push @$result, $self->eval_attr_value
                ($node, 'value', disallow_undef => 'w', required => 'm',
                 node_info => $process->{node_info},
                 destination => 'print');
            }
            next;
          } else { # $ln
            next if $self->_close_start_tag ($process => $processes => $result);

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

          #XXX
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
          next if $self->_close_start_tag ($process => $processes => $result);
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
            push @$result, '</head>';
          }
        }

        $self->_before_non_space ($process => $result);

        my $node_info = {node => $node, attrs => {},
                         binds => $process->{node_info}->{binds},
                         fields => $process->{node_info}->{fields},
                         macro_depth => $process->{node_info}->{macro_depth}};

        if (defined $ln and $ln =~ /\A[A-Za-z_-][A-Za-z0-9_-]*\z/) {
          push @$result, '<' . $ln;
          $self->{current_tag} = $node_info;
          $node_info->{ns} = $ns;
          $node_info->{ln} = $ln;
          $node_info->{lnn} = $ln;
          $node_info->{lnn} =~ tr/A-Z/a-z/; ## ASCII case-insensitive.

          if ($node_info->{ns} eq HTML_NS and
              $Web::HTML::ParserData::AllVoidElements->{$node_info->{lnn}}) {
            unshift @{$processes}, {type => 'end start tag'};

            $self->_print_attrs ($attrs, $node_info => $result);

            unshift @{$processes},
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

            unshift @{$processes},
                {type => 'end tag', node_info => $node_info};
            
            $self->_print_attrs ($attrs, $node_info => $result);

            unshift @{$processes},
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
            unshift @{$processes},
                {type => 'end tag', node_info => {ln => 'head'}};
          }

          unshift @{$processes}, {type => 'end start tag'};
          unshift @{$processes},
              map { {type => 'node', node => $_, node_info => $node_info} } 
              grep { $_->node_type == ELEMENT_NODE or
                     $_->node_type == TEXT_NODE }
              @{$node->child_nodes->to_a};
        }
        $self->{need_head_end_tag}-- if $self->{need_head_end_tag};
      } elsif ($nt == DOCUMENT_TYPE_NODE) {
        next if $self->_close_start_tag ($process => $processes => $result);

        my $nn = $node->node_name;
        $nn =~ s/[^0-9A-Za-z_-]/_/g;
        push @$result, '<!DOCTYPE ' . $nn . '>';
      } elsif ($nt == DOCUMENT_NODE) {
        my $node_info = {allow_children => 1};
        unshift @{$processes},
            map { {type => 'node', node => $_, node_info => $node_info} } 
            grep { $_->node_type == ELEMENT_NODE or
                   $_->node_type == DOCUMENT_TYPE_NODE }
            @{$node->child_nodes->to_a};
      } else {
        die "Unknown node type |$nt|";
      }

    } elsif ($process->{type} eq 'end tag') {
      next if $self->_close_start_tag ($process => $processes => $result);

      if ($process->{node_info}->{comment}) {
        push @$result, ['printctext'], "-->";
        next;
      } elsif ($process->{node_info}->{rawtext}) {
        push @$result, ['printrawtext', $process->{node_info}->{ln}];
      }

      if ($process->{node_info}->{ln} eq 'head') {
        $self->{need_head_end_tag}++;
      } else {
        while (($self->{need_head_end_tag} || 0) > 0) {
          push @$result, '</head>';
          $self->{need_head_end_tag}--;
        }
        push @$result, '</' . $process->{node_info}->{ln} . '>';
      }
    } elsif ($process->{type} eq 'text') {
      if ($process->{node_info}->{rawtext}) {
        push @$result, ['rawtext', $process->{value}];
      } elsif ($process->{node_info}->{plaintext}) {
        push @$result, $process->{value};
      } else {
        push @$result, htescape $process->{value};
      }
    } elsif ($process->{type} eq 'end start tag') {
      next if $self->_close_start_tag ($process => $processes => $result);
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  } # @{$processes}

    return not 'done';
  };

  })->then (sub {
    return $result;
  });
} # _compile

sub _print_attrs ($$$$) {
  my ($self, $attrs, $node_info, $result) = @_;
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
    my $eval;
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
    } elsif ($attr_ns eq TEMMA_PERL_NS) {
      $eval = 1;
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
      if ($eval) {
        push @$result, $self->eval_attr_value
            ($attr->owner_element, $attr->node_name,
             attr_node => $attr,
             node_info => $node_info,
             destination => 'classes');
      } else {
        push @$result, ['addclasses', [grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $value]];
      }
    } else {
      if ($eval) {
        push @$result, $self->eval_attr_value
            ($attr->owner_element, $attr->node_name,
             attr_node => $attr,
             node_info => $node_info,
             destination => 'addattr',
             attr_name => $attr_name);
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
  }
  push @$result, join '', @attr if @attr;
} # _print_attrs

sub _close_start_tag ($$$$) {
  my ($self, $current_process, $processes, $result) = @_;
  return 0 unless my $node_info = delete $self->{current_tag};
  return 0 if $node_info->{ln} eq '';

  push @$result, ['endofstarttag'], '>';
  unshift @$processes, $current_process;

  if ($Temma::Defs::IgnoreFirstNewline->{$node_info->{ln}} and
      $node_info->{ns} eq HTML_NS) {
    push @$result, "\x0A";
  }

  return 1;
} # _close_start_tag

sub _before_non_space ($$;%) {
  my ($self, $process, $result, %args) = @_;
  if (defined $process->{node_info}->{trailing_space}) {
    if ($process->{node_info}->{has_non_space}) {
      if ($process->{node_info}->{rawtext}) {
        push @$result, ['rawtext', $process->{node_info}->{trailing_space}];
      } elsif ($process->{node_info}->{plaintext}) {
        push @$result, $process->{node_info}->{trailing_space};
      } else {
        push @$result, htescape $process->{node_info}->{trailing_space};
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

  my $opts = {};
  if ($args{destination} eq 'attr') {
    $opts->{used_names} = $args{node_info}->{attrs};
    $opts->{ns} = $self->{current_tag}->{ns};
  } elsif ($args{destination} eq 'addattr') {
    $opts->{attr_name} = $args{attr_name};
    $opts->{used_names} = $args{node_info}->{attrs};
  }

  return ['eval', $location, $attr_node->value,
          $args{context} // '', $args{disallow_undef},
          $args{node_info}->{binds} || {},
          $args{destination}, $opts];
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

  #XXX
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

sub evaluate ($$$;%) {
  my ($self, $compiled, $ws, %args) = @_;
  my $writer = $ws->get_writer;
  return Promise->resolve->then (sub {
    my @op = @$compiled;
    return promised_until {
      return 'done' unless @op;
      while (@op) {
        if (not ref $op[0]) {
          $writer->write (\($op[0]));
          shift @op;
          next;
        }

        my $x = shift @op;
        if ($x->[0] eq 'eval') {
          local $_ = $x->[5]; # XXX
          my $value = qq{local \$_;\n#line 1 "$x->[1]"\n$x->[2];};
          if (keys %$_) {
            $value = "return do {\n$value\n};";
            for my $var (keys %$_) {
              $value = qq{for my \$$var (\$_->{$var}->[0]->[$_->{$var}->[1]]) {\n$value\n}\n};
            }
            $value = qq{#line 1 "vars for $x->[1]"\n} . $value;
          }
          $self->{eval_package} ||= qq{Temma::Eval::@{[time]}::@{[int rand 100000]}};
          $value = qq{package $self->{eval_package};\n$value};

          my $evaled;
          my $error;
          my $context = $x->[3];
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
            #$exception->source_node ($attr_node);
            # XXX location $x->[1]
            die $exception;
          }

          if ($x->[4] and not defined $evaled) {
            $self->onerror->(type => 'temma:undef',
                             level => $x->[4]);
            #node => $attr_node);
            # XXX throw if level is m
          }

          if (not defined $evaled) {
            #
          } elsif ($x->[6] eq 'attr_name') {
            $self->{current}->{attr_name} = $evaled;
          } elsif ($x->[6] eq 'attr') {

            #XXXif ($self->{current_tag}) {
            #} else {
            #  $self->onerror->(type => 'temma:start tag already closed',
            #                   node => $node,
            #                   level => 'm');
            #}
            #next if $self->{current_tag}->{ln} eq '';

            my $attr_name = delete $self->{current}->{attr_name};
            $attr_name = '' unless defined $attr_name;
            $attr_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
            my $element_ns = $x->[7]->{ns};
            if ($element_ns eq SVG_NS) {
              $attr_name = $Web::HTML::ParserData::SVGAttrNameFixup->{$attr_name} || $attr_name;
            } elsif ($element_ns eq MML_NS) {
              $attr_name = $Web::HTML::ParserData::MathMLAttrNameFixup->{$attr_name} || $attr_name;
            }
            ## $Web::HTML::ParserData::ForeignAttrNamespaceFixup is
            ## ignored here as the mapping is no-op for the purpose of
            ## qualified name serialization.

            unless ($attr_name =~ /\A[A-Za-z_-][A-Za-z0-9_:-]*\z/) {
              $self->onerror->(type => 'temma:name not serializable',
                               #node => $node,
                               value => $attr_name,
                               level => 'm');
              next;
            }

            if ($x->[7]->{used_names}->{$attr_name}) {
              $self->onerror->(type => 'temma:duplicate attr',
                               #node => $node,
                               value => $attr_name,
                               level => 'm');
              next;
            }

            if ($attr_name eq 'class') {
              push @{$self->{current}->{classes} ||= []}, 
                  grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $evaled;
            } else {
              $x->[7]->{used_names}->{$attr_name} = 1;
              $writer->write (\(' ' . $attr_name . '="' . (htescape $evaled) . '"'));
            }
          } elsif ($x->[6] eq 'addattr') {
            my $attr_name = $x->[7]->{attr_name};
            if ($x->[7]->{used_names}->{$attr_name}) {
              $self->onerror->(type => 'temma:duplicate attr',
                               #node => $node,
                               value => $attr_name,
                               level => 'm');
              next;
            }

            $x->[7]->{used_names}->{$attr_name} = 1;
            $writer->write (\(' ' . $attr_name . '="' . (htescape $evaled) . '"'));
          } elsif ($x->[6] eq 'classes') {
            # XXX if no current tag
            push @{$self->{current}->{classes} ||= []}, 
                grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $evaled;
          } elsif ($x->[6] eq 'print') {
            $writer->write (\$evaled);
          } elsif ($x->[6] eq 'printescaped') {
            $writer->write (\(htescape $evaled));
          } elsif ($x->[6] eq 'rawtext') {
            $self->{current}->{rawtext} .= $evaled;
          } elsif ($x->[6] eq 'openstart') {
            #XXX
          } elsif ($x->[6] eq 'nop') {
            #
          } else {
            die "Bad destination |$x->[6]|";
          }
        } elsif ($x->[0] eq 'addclasses') {
          push @{$self->{current}->{classes} ||= []}, @{$x->[1]};
        } elsif ($x->[0] eq 'endofstarttag') {
          if (@{$self->{current}->{classes} or []}) {
            $writer->write (\(
                q< class="> .
                (htescape join ' ', @{delete $self->{current}->{classes}}) .
                q<">
            ));
          }
        } elsif ($x->[0] eq 'rawtext') {
          $self->{current}->{rawtext} .= $x->[1];
        } elsif ($x->[0] eq 'printrawtext') {
          # XXX run compiletime when possible
          my $value = delete $self->{current}->{rawtext};
          if ($value =~ m{</}) {
            my $doc = Web::DOM::Document->new;
            $doc->manakai_is_html (1);

            my $el = $doc->create_element ('div');
            my $ln = $x->[1];
            $el->inner_html ('<' . $ln . '>' . $value . '</' . $ln . '>');
            my $value2 = $el->first_child->text_content;
            if ($value ne $value2) {
              $self->onerror->(type => 'temma:not representable in raw text',
                               #node => $process->{node_info}->{node},
                               level => 'm');
              next;
            }
          }
          $writer->write (\$value);
        } elsif ($x->[0] eq 'printctext') {
          my $value = delete $self->{current}->{rawtext};
          $value =~ s/--/- - /g;
          $value =~ s/-\z/- /;
          $writer->write (\$value);
        } else {
          #XXX
        }
      } # while
      return not 'done';
    }; # until
  })->finally (sub {
    return $writer->close;
  });
} # evaluate

=pod

XXX
  if (not ref $vv) {

  } elsif ($vv->[0] eq 'openstart') {
    XXX
    my $ln = evaluated $vn->[1]
            if (defined $ln) {
              $ln =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
              $ns = $node->$TemmaContextNode->manakai_get_child_namespace_uri ($ln);
              if ($ns eq SVG_NS) {
                $ln = $Web::HTML::ParserData::SVGElementNameFixup->{$ln} || $ln;
              }
            }
    # XXX
  } elsif ($v->[0] eq 'if') {
  } elsif ($v->[0] eq 'for') {
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
              unshift @{$processes},
                  {type => 'for block',
                   nodes => $nodes,
                   sep_nodes => $sep_nodes,
                   node_info => $process->{node_info},
                   space => 
                   sep_space => $sep_node ? _ascii_lc $sep_node->get_attribute_ns (TEMMA_NS, 'space') || '' : undef,
                   items => $items,
                   index => 0,
                   bound_to => $as,
                   block_name => $block_name};
  } elsif ($v->[0] eq 'my') {
  } elsif ($v->[0] eq 'content') {
            my $def = $process->{node_info}->{fields}->{$name};
            next unless $def;

            $self->_schedule_nodes
              ([grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$def->{node}->child_nodes->to_a}],
               $process->{node_info}, $def->{sp}, binds => $def->{binds});
  } elsif ($x->[0] eq 'include') {

            my $x = {
              context => $node,
              path => $path,
              doc_to_path => $self->{doc_to_path},
              onerror => $self->onerror,
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
                      $self->_print_attrs ($attrs, $self->{current_tag});
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
              $self->_compile;
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
  } elsif ($x->[0] eq 'next') {
  } elsif ($x->[0] eq 'last') {

            my $found;
            my $searched = $ln eq 'last' ? 'for block' : 'end block';
            my @close;
            while (@{$processes}) {
              my $process = shift @{$processes};
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
              unshift @{$processes}, @close;
            } else {
              $self->onerror->(type => 'temma:block not found',
                               value => $block_name,
                               node => $node,
                               level => 'm');
            }

  } else {

  }


sub _compile ($) {
  my ($self) = @_;
  A: {
    eval {
      $self->__compile;
    };
    #XXX
    if ($@) {
      my $exception = $@;
      if (UNIVERSAL::isa ($exception, 'Temma::Exception')) {
        my $catch;
        my @close;
        while (@{$processes}) {
          my $process = shift @{$processes};
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
        unshift @{$processes}, @close;
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
        unshift @{$processes}, $close;
        redo A;
      } else {
        $self->_cleanup;
        die $exception;
      }
    }
  } # A
} # _compile
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
  
  unshift @{$processes},
      {type => 'end block', node_info => $node_info,
       parent_node_info => $parent_node_info,
       block_name => $args{block_name},
       catches => $args{catches},
       is_entity_boundary => $args{is_entity_boundary}};
  unshift @{$processes},
      map { {type => 'node', node => $_, node_info => $node_info} } @$nodes;
} # _schedule_nodes
    } elsif ($process->{type} eq 'for block') {
      my $index = $process->{index};
      next unless defined $index; # end of for
      my $block_name = $process->{block_name};
      if (++$process->{index} <= $#{$process->{items}}) {
        unshift @{$processes}, $process;
        $self->_schedule_nodes
            ($process->{sep_nodes}, $process->{node_info},
             {preserve => 'preserve', trim => 'trim'}->{$process->{sep_space}} || $process->{space},
             binds => $process->{node_info}->{binds})
                if @{$process->{sep_nodes}};
      } else {
        unshift @{$processes},
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

sub _cleanup ($) {
  my $self = $_[0];
  if (defined $self->{eval_package}) {
    delete_package $self->{eval_package};
  }
} # _cleanup

=cut

1;

=head1 LICENSE

Copyright 2012-2019 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
