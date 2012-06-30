package Temma::Processor;
use strict;
use warnings;
no warnings 'utf8';

sub _eval ($) {
  return eval ('local $_; local @_;' . "\n" . $_[0]);
} # _eval

our $VERSION = '1.0';
use Message::DOM::Node;
use Temma::Defs;

sub new ($) {
  return bless {
    onerror => sub {
      my %args = @_;
      my $msg = $args{type};
      $msg .= ' (' . $args{value} . ')' if defined $args{value};
      if ($args{node}) {
        $msg .= ' at node ' . $args{node}->manakai_local_name;
        my $node = $args{node};
        while ($node) {
          my $line = $node->get_user_data ('manakai_source_line');
          my $column = $node->get_user_data ('manakai_source_column');
          if (defined $line and defined $column) {
            $msg .= sprintf ' at line %d column %d',
                $line || 0, $column || 0;
            last;
          }
          $node = $node->parent_node;
        }
      }
      warn "$msg\n";
    },
  }, $_[0];
} # new

sub onerror ($;$) {
  if (@_ > 1) {
    $_[0]->{onerror} = $_[1];
  }
  return $_[0]->{onerror};
} # onerror

sub htescape ($) {
  return $_[0] unless $_[0] =~ /[&"<>]/;
  my $s = $_[0];
  $s =~ s/&/&amp;/g;
  $s =~ s/"/&quot;/g;
  $s =~ s/</&lt;/g;
  $s =~ s/>/&gt;/g;
  return $s;
} # htescape

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

sub process_document ($$$) {
  my ($self, $doc => $fh) = @_;

  $self->{processes} = [];
  push @{$self->{processes}},
      {type => 'node', node => $doc, node_info => {allow_children => 1}},
      {type => 'end'};

  $self->_process ($fh);
} # process_document

sub _process ($$) {
  my ($self, $fh) = @_;

  eval {
    $self->__process ($fh);
  };
  if ($@) {
    if (UNIVERSAL::isa ($@, 'Temma::Exception')) {
      #warn $@->source_text;
      $self->{onerror}->(type => 'temma:perl exception',
                         level => 'm',
                         value => $@,
                         node => $@->source_node);
    } else {
      die $@;
    }
  }
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
          unless ($process->{node_info}->{has_non_space}) {
            $data =~ s/^[\x09\x0A\x0C\x0D\x20]+//;
            $process->{node_info}->{has_non_space} = 1;
          }
        } else {
          ## White space only Text node
          if ($process->{node_info}->{preserve_space}) {
            next if $self->_close_start_tag ($process, $fh);
          }
          if ($self->{current_tag}) {
            ## Ignore white space characters between t:attr elements.
            next;
          }
        }

        unless ($process->{node_info}->{allow_children}) {
          unless ($process->{node_info}->{children_not_allowed_error}) {
            $self->{onerror}->(type => 'temma:child not allowed',
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

        if ($process->{node_info}->{rawtext}) {
          ${$process->{node_info}->{rawtext_value}} .= $data;
        } else {
          print $fh htescape $data;
        }
      } elsif ($nt == ELEMENT_NODE) {
        my $ns = $node->namespace_uri || '';
        my $ln = $node->manakai_local_name;
        my $attrs = [];
        if ($ns eq TEMMA_NS) {
          if ($ln eq 'text') {
            next if $self->_close_start_tag ($process, $fh);
            $self->_before_non_space ($process => $fh);
            
            my $value = $self->eval_attr_value
                ($node, 'value', disallow_undef => 'w', required => 'm');
            if (defined $value) {
              if ($process->{node_info}->{rawtext}) {
                ${$process->{node_info}->{rawtext_value}} .= $value;
              } else {
                print $fh htescape $value;
              }
            }
            next;
          } elsif ($ln eq 'element') {
            next if $self->_close_start_tag ($process, $fh);

            $ln = $self->eval_attr_value ($node, 'name');
            $ln = '' unless defined $ln;
            $ln =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
            $ns = $node->$TemmaContextNode->manakai_get_child_namespace_uri ($ln);
            if ($ns eq SVG_NS) {
              $ln = $Whatpm::HTML::ParserData::SVGElementNameFixup->{$ln} || $ln;
            }
          } elsif ($ln eq 'attr' or $ln eq 'class') {
            if ($self->{current_tag}) {
              my $attr_name = $ln eq 'class' ? 'class' :
                  $self->eval_attr_value ($node, 'name');
              $attr_name = '' unless defined $attr_name;
              $attr_name =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
              my $element_ns = $self->{current_tag}->{ns};
              if ($element_ns eq SVG_NS) {
                $attr_name = $Whatpm::HTML::ParserData::SVGAttrNameFixup->{$attr_name} || $attr_name;
              } elsif ($element_ns eq MML_NS) {
                $attr_name = $Whatpm::HTML::ParserData::MathMLAttrNameFixup->{$attr_name} || $attr_name;
              }
              ## $Whatpm::HTML::ParserData::ForeignAttrNamespaceFixup
              ## is ignored here as the mapping is no-op for the
              ## purpose of qualified name serialization.

              unless ($attr_name =~ /\A[A-Za-z_-][A-Za-z0-9_:-]*\z/) {
                $self->{onerror}->(type => 'temma:name not serializable',
                                   node => $node,
                                   value => $attr_name,
                                   level => 'm');
                next;
              }

              if ($self->{current_tag}->{attrs}->{$attr_name}) {
                $self->{onerror}->(type => 'temma:duplicate attr',
                                   node => $node,
                                   value => $attr_name,
                                   level => 'm');
                next;
              }

              my $value = $self->eval_attr_value
                  ($node, $ln eq 'class' ? 'name' : 'value',
                   disallow_undef => 'w', required => 'm');
              if (defined $value) {
                if ($attr_name eq 'class') {
                  push @{$self->{current_tag}->{classes} ||= []}, 
                      grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $value;
                } else {
                  $self->{current_tag}->{attrs}->{$attr_name} = 1;
                  print $fh ' ' . $attr_name . '="' . (htescape $value) . '"';
                }
              }
            } else {
              $self->{onerror}->(type => 'temma:start tag already closed',
                                 node => $node,
                                 level => 'm');
            }
            next;
          } elsif ($ln eq 'comment') {
            next if $self->_close_start_tag ($process, $fh);

            if ($process->{node_info}->{rawtext}) {
              $self->{onerror}->(type => 'temma:comment not allowed',
                                 node => $node,
                                 level => 'm');
              next;
            }

            $self->_before_non_space ($process => $fh);

            my $node_info = {rawtext => 1, rawtext_value => \(my $v = ''),
                             allow_children => 1, comment => 1,
                             has_non_space => 1, preserve_space => 1};

            unshift @{$self->{processes}},
                {type => 'end tag', node_info => $node_info};

            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} }
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node->child_nodes->to_a};

            print $fh "<!--";
            next;

          } elsif ($ln eq 'if') {
            $self->_before_non_space ($process => $fh, transparent => 1);

            my $value = $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'bool');

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
                      ($_, 'x', required => 'm', context => 'bool');
                  if ($value) {
                    $state = 'if-matched';
                    $cond_node = $_;
                  }
                } elsif ($state eq 'if-matched') {
                  last;
                } else {
                  $self->{onerror}->(type => 'element not allowed:t:if',
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
                  $self->{onerror}->(type => 'element not allowed:t:if',
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

            my $sp = $cond_node->get_attribute_ns (TEMMA_NS, 'space') || '';
            $self->_schedule_nodes (\@node, $process->{node_info}, $sp);
            next;
          } elsif ($ln eq 'for') {
            $self->_before_non_space ($process => $fh, transparent => 1);

            my $items = $self->eval_attr_value
                ($node, 'x', required => 'm', disallow_undef => 'm');
            $items = [] unless defined $items;
            my $item_count = do {
              local $@;
              eval { 0+@$items };
            };
            if (not defined $item_count) {
              $self->{onerror}->(type => 'temma:not arrayref',
                                 node => $node->get_attribute_node ('x'),
                                 level => 'm');
              $items = [];
              $item_count = 0;
            }

            my $nodes = [
              grep { $_->node_type == ELEMENT_NODE or
                     $_->node_type == TEXT_NODE }
              @{$node->child_nodes->to_a}
            ];

            # XXX |as| attribute
            
            my $sp = $node->get_attribute_ns (TEMMA_NS, 'space') || '';
            if ($item_count > 0) {
              unshift @{$self->{processes}},
                  {type => 'for block',
                   nodes => $nodes,
                   node_info => $process->{node_info},
                   space => $sp,
                   items => $items,
                   index => 0};
            }
            next;
          } elsif ($ln eq 'call') {
            $self->eval_attr_value
                ($node, 'x', required => 'm', context => 'void');
            next;
          } elsif ($ln eq 'wait') {
            my $value = $self->eval_attr_value
                  ($node, 'cv', disallow_undef => 'm', required => 'm');
            if (not defined $value) {
              #
            } elsif (not UNIVERSAL::can ($value, 'cb')) {
              $self->{onerror}->(type => 'temma:no cb method',
                                 level => 'm',
                                 node => $node->get_attribute_node ('cv'));
            } else {
              eval {
                $value->cb (sub {
                  unshift @{$self->{processes}},
                      {type => 'eval_attr_value',
                       node => $node, attr_name => 'cb'};
                  $self->_process ($fh);
                });
                1;
              } or do {
                $self->{onerror}->(type => 'temma:perl exception:cb',
                                   level => 'm',
                                   value => $@,
                                   node => $node->get_attribute_node ('cv'));
              };
              return;
            }
            next;
          } else {
            next if $self->_close_start_tag ($process, $fh);

            if ($ln eq 'else' or $ln eq 'elsif') {
              $self->{onerror}->(type => 'element not allowed',
                                 node => $node,
                                 level => 'm');
            } else { ## Unknown element in Temma namespace
              $self->{onerror}->(type => 'temma:unknown element',
                                 node => $node,
                                 value => $ln,
                                 level => 'm');
            }
            next;
          }
        } else {
          next if $self->_close_start_tag ($process, $fh);
          $attrs = $node->attributes;
        }

        if (not $process->{node_info}->{allow_children} or
            $process->{node_info}->{rawtext}) {
          unless ($process->{node_info}->{children_not_allowed_error}) {
            $self->{onerror}->(type => 'temma:child not allowed',
                               node => $node,
                               level => 'm');
            $process->{node_info}->{children_not_allowed_error} = 1;
          }
          next;
        }

        $self->_before_non_space ($process => $fh);

        if ($ln =~ /\A[A-Za-z_-][A-Za-z0-9_-]*\z/) {
          print $fh '<' . $ln;
          my $node_info = $self->{current_tag} =
              {node => $node, ns => $ns, ln => $ln, lnn => $ln, attrs => {}};
          $node_info->{lnn} =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
          
          for my $attr (@$attrs) {
            ## Note that if there are multiple attributes with only
            ## case differences in their names, or with same qualified
            ## name but in different namespaces, then, in HTML
            ## serialization, the only first one has effect (and the
            ## name is interpreted ASCII case-insensitively by
            ## parser).  We don't try to detect such an error as it is
            ## unlikely happens in typical use cases.

            my $attr_ns = $attr->namespace_uri;
            my $attr_name;
            if (not defined $attr_ns) {
              $attr_name = $attr->manakai_local_name;
            } elsif ($attr_ns eq XML_NS) {
              $attr_name = 'xml:' . $attr->manakai_local_name;
            } elsif ($attr_ns eq XMLNS_NS) {
              $attr_name = 'xmlns:' . $attr->manakai_local_name;
              $attr_name = 'xmlns' if $attr_name eq 'xmlns:xmlns';
            } elsif ($attr_ns eq XLINK_NS) {
              $attr_name = 'xlink:' . $attr->manakai_local_name;
            } elsif ($attr_ns eq TEMMA_NS) {
              next;
            } else {
              $attr_name = $attr->name;
            }

            unless ($attr_name =~ /\A[A-Za-z_-][A-Za-z0-9_:-]*\z/) {
              $self->{onerror}->(type => 'temma:name not serializable',
                                 node => $attr,
                                 value => $attr_name,
                                 level => 'm');
              next;
            }

            if ($attr_name eq 'class') {
              $node_info->{classes} = [grep { length } split /[\x09\x0A\x0C\x0D\x20]+/, $attr->value];
            } else {
              $node_info->{attrs}->{$attr_name} = 1;
              print $fh ' ' . $attr_name . '="' . (htescape $attr->value) . '"';
            }
          }

          if ($node_info->{ns} eq HTML_NS and
              $Whatpm::HTML::ParserData::AllVoidElements->{$node_info->{lnn}}) {
            unshift @{$self->{processes}}, {type => 'end'};

            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} } 
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node_info->{node}->child_nodes->to_a};
          } else {
            $node_info->{allow_children} = 1;
            if ($node_info->{ns} eq HTML_NS and
                ($node_info->{lnn} eq 'script' or
                 $node_info->{lnn} eq 'style')) {
              $node_info->{rawtext} = 1;
              $node_info->{rawtext_value} = \(my $v = '');
            }

            if ((($Temma::Defs::PreserveWhiteSpace->{$node_info->{ns}}->{$node_info->{ln}} or
                  $process->{node_info}->{preserve_space}) and
                 not (($node->get_attribute_ns (TEMMA_NS, 'space') || '') eq 'trim')) or
                ($node->get_attribute_ns (TEMMA_NS, 'space') || '') eq 'preserve') {
              $node_info->{preserve_space} = 1; # for descendants
              $node_info->{has_non_space} = 1; # for children
            }

            unshift @{$self->{processes}},
                {type => 'end tag', node_info => $node_info};
            
            unshift @{$self->{processes}},
                map { {type => 'node', node => $_, node_info => $node_info} } 
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node_info->{node}->child_nodes->to_a};
          }
        } else {
          ## The element is not in the temma namespace and its local
          ## name is not serializable.  The element is ignored but its
          ## content is processed.

          $self->{onerror}->(type => 'temma:name not serializable',
                             node => $node,
                             value => $ln,
                             level => 'm');

          my $node_info = {allow_children => 1};
          unshift @{$self->{processes}},
              map { {type => 'node', node => $_,
                     node_info => $node_info} } 
              grep { $_->node_type == ELEMENT_NODE or
                     $_->node_type == TEXT_NODE }
              @{$node->child_nodes->to_a};
        }
      } elsif ($nt == DOCUMENT_TYPE_NODE) {
        next if $self->_close_start_tag ($process, $fh);

        my $nn = $node->node_name;
        $nn =~ s/[^0-9A-Za-z_-]/_/g;
        print $fh '<!DOCTYPE ' . $nn . '>';
      } elsif ($nt == DOCUMENT_NODE) {
        my $node_info = {allow_children => 1};
        unshift @{$self->{processes}},
            map { {type => 'node', node => $_,
                   node_info => $node_info} } 
            grep { $_->node_type == ELEMENT_NODE or
                   $_->node_type == DOCUMENT_TYPE_NODE }
            @{$node->child_nodes->to_a};

        # XXX second element? text children?
      } else {
        die "Unknown node type |$nt|";
      }

    } elsif ($process->{type} eq 'end tag') {
      next if $self->_close_start_tag ($process, $fh);

      if ($process->{node_info}->{comment}) {
        my $value = ${$process->{node_info}->{rawtext_value}};
        $value =~ s/--/- - /g;
        $value =~ s/-\z/- /;
        print $fh $value, "-->";
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
          $self->{onerror}->(type => 'temma:not representable in raw text',
                             node => $process->{node_info}->{node},
                             level => 'm');
        }
        
        print $fh $value2;
      }

      print $fh '</' . $process->{node_info}->{ln} . '>';
    } elsif ($process->{type} eq 'for block') {
      my $index = $process->{index};
      if (++$process->{index} <= $#{$process->{items}}) {
        unshift @{$self->{processes}}, $process;
      }
      
      # XXX $index
      $self->_schedule_nodes
          ($process->{nodes}, $process->{node_info}, $process->{space});
    } elsif ($process->{type} eq 'end block') {
      $process->{parent_node_info}->{has_non_space} = 1
          if $process->{node_info}->{has_non_space};
    } elsif ($process->{type} eq 'eval_attr_value') {
      $self->eval_attr_value ($process->{node}, $process->{attr_name});
    } elsif ($process->{type} eq 'end') {
      next if $self->_close_start_tag ($process, $fh);
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  } # @{$self->{processes}}
} # _process

## Schedule processing of nodes, used for processing of <t:if>,
## <t:for>, and <t:try>.
sub _schedule_nodes ($$$$) {
  my ($self, $nodes, $parent_node_info, $sp) = @_;

  my $node_info = {
    %{$parent_node_info},
    trailing_space => '',
  };
  $node_info->{preserve_space}
      = $sp eq 'preserve' ? 1 :
        $sp eq 'trim' ? 0 :
        $parent_node_info->{preserve_space};
  $node_info->{has_non_space} = $node_info->{preserve_space};
  
  unshift @{$self->{processes}},
      {type => 'end block', node_info => $node_info,
       parent_node_info => $parent_node_info};
  unshift @{$self->{processes}},
      map { {type => 'node', node => $_, node_info => $node_info} } @$nodes;
} # _schedule_nodes

sub _close_start_tag ($$$) {
  my ($self, $current_process, $fh) = @_;
  return 0 unless my $node_info = delete $self->{current_tag};
  
  if (@{$node_info->{classes} or []}) {
    print $fh q< class=">;
    print $fh htescape join ' ', @{$node_info->{classes}};
    print $fh q<">;
  }
  print $fh '>';
  unshift @{$self->{processes}}, $current_process;

  if ($Temma::Defs::IgnoreFirstNewline->{$node_info->{ln}} and
      $node_info->{ns} eq HTML_NS) {
    print $fh "\x0A";
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
      } else {
        print $fh htescape $process->{node_info}->{trailing_space};
      }
    }
    delete $process->{node_info}->{trailing_space};
  }
  $process->{node_info}->{has_non_space} = 1 unless $args{transparent};
} # _before_non_space

sub eval_attr_value ($$$;%) {
  my ($self, $node, $name, %args) = @_;
  
  my $attr_node = $node->get_attribute_node ($name) or do {
    if ($args{required}) {
      $self->{onerror}->(type => 'attribute missing',
                         text => $name,
                         level => $args{required},
                         node => $node);
    }
    return undef;
  };

  my $location = $node->node_name . '[' . $name . ']';
  my $parent = $node->parent_node;
  while ($parent) {
    $location = $parent->node_name . '>' . $location
        if $parent->node_type == ELEMENT_NODE;
    $parent = $parent->parent_node;
  }
  my $line = $attr_node->get_user_data ('manakai_source_line')
      || $node->get_user_data ('manakai_source_line');
  my $column = $attr_node->get_user_data ('manakai_source_column')
      || $node->get_user_data ('manakai_source_column');
  if (defined $line and defined $column) {
    $location .= sprintf ' (at line %d column %d)', $line || 0, $column || 0;
  }
  $location =~ s/[\x00-\x1F\x22]+/ /g;
  my $value = qq<#line 1 "$location"\n> . $attr_node->value;

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
    $self->{onerror}->(type => 'temma:undef',
                       level => $args{disallow_undef},
                       node => $attr_node);
  }

  return $evaled;
} # eval_attr_value

1;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
