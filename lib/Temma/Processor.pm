package Temma::Processor;
use strict;
use warnings;
no warnings 'utf8';
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

  while (@{$self->{processes}}) {
    my $process = shift @{$self->{processes}};

    if ($process->{type} eq 'node') {
      my $node = $process->{node};
      my $nt = $node->node_type;
      if ($nt == TEXT_NODE) {
        my $data = $node->data;
        if ($data =~ /[^\x09\x0A\x0C\x0D\x20]/) {
          next if $self->_close_start_tag ($process, $fh);
        } elsif ($self->{current_tag}) {
          ## Ignore white space characters between t:attr elements.
          next;
        }

        # XXX interspace white space consideration

        unless ($process->{node_info}->{allow_children}) {
          unless ($process->{node_info}->{children_not_allowed_error}) {
            $self->{onerror}->(type => 'temma:child not allowed',
                               node => $node,
                               level => 'm');
            $process->{node_info}->{children_not_allowed_error} = 1;
          }
          next;
        }

        print $fh htescape $node->data;
      } elsif ($nt == ELEMENT_NODE) {
        my $ns = $node->namespace_uri || '';
        my $ln = $node->manakai_local_name;
        my $attrs = [];
        if ($ns eq TEMMA_NS) {
          if ($ln eq 'element') {
            next if $self->_close_start_tag ($process, $fh);

            $ln = $self->eval_attr_value ($node, 'name');
            $ln = '' unless defined $ln;
            $ln =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
            $ns = $node->$TemmaContextNode->manakai_get_child_namespace_uri ($ln);
            if ($ns eq SVG_NS) {
              $ln = $Whatpm::HTML::ParserData::SVGElementNameFixup->{$ln} || $ln;
            }
          } elsif ($ln eq 'attr') {
            if ($self->{current_tag}) {
              my $attr_name = $self->eval_attr_value ($node, 'name'); # XXX case
              unless ($self->{current_tag}->{attrs}->{$attr_name}) {
                $self->{current_tag}->{attrs}->{$attr_name} = 1;
                my $value = $self->eval_attr_value ($node, 'value'); # XXX element content?
                print $fh ' ' . $attr_name . '="' . (htescape $value) . '"';
              } else {
                $self->{onerror}->(type => 'temma:duplicate attr',
                                   node => $node,
                                   value => $attr_name,
                                   level => 'm');
              }
            } else {
              $self->{onerror}->(type => 'temma:start tag already closed',
                                 node => $node,
                                 level => 'm');
            }
            next;
          } else {
            next if $self->_close_start_tag ($process, $fh);

            $self->{onerror}->(type => 'temma:unknown element',
                               node => $node,
                               value => $ln,
                               level => 'm');
            next;
          }
        } else {
          next if $self->_close_start_tag ($process, $fh);
          $attrs = $node->attributes;
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

        if ($ln =~ /\A[A-Za-z_-][A-Za-z0-9_-]*\z/) {
          print $fh '<' . $ln;
          my $node_info = $self->{current_tag} =
              {node => $node, ns => $ns, ln => $ln, lnn => $ln, attrs => {}};
          $node_info->{lnn} =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
          
          for my $attr (@$attrs) {
            my $attr_name = $attr->node_name; # XXX
            next if $attr_name =~ /^t:/; # XXX

            $node_info->{attrs}->{$attr_name} = 1;
            print $fh ' ' . $attr_name . '="' .
                (htescape $attr->node_value) . '"';
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
            unshift @{$self->{processes}},
                {type => 'end tag', node_info => $node_info};
            
            $node_info->{allow_children} = 1;
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

          # XXX attrs should be ignored

          unshift @{$self->{processes}},
              map { {type => 'node', node => $_,
                     node_info => {allow_children => 1}} } 
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
        unshift @{$self->{processes}},
            map { {type => 'node', node => $_,
                   node_info => {allow_children => 1}} } 
            grep { $_->node_type == ELEMENT_NODE or
                   $_->node_type == DOCUMENT_TYPE_NODE }
            @{$node->child_nodes->to_a};

        # XXX second element? text children?
      } else {
        die "Unknown node type |$nt|";
      }

    } elsif ($process->{type} eq 'end tag') {
      next if $self->_close_start_tag ($process, $fh);

      print $fh '</' . $process->{node_info}->{ln} . '>';
    } elsif ($process->{type} eq 'end') {
      next if $self->_close_start_tag ($process, $fh);
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  }
} # proess_document

sub _close_start_tag ($$$) {
  my ($self, $current_process, $fh) = @_;
  return 0 unless my $node_info = delete $self->{current_tag};
  
  unshift @{$self->{processes}}, $current_process;
  print $fh '>';
  return 1;
} # _close_start_tag

sub eval_attr_value ($$$) {
  my ($self, $node, $name) = @_;
  
  my $value = $node->get_attribute ($name);
  return undef if not defined $value;

  my $evaled;
  my $error;
  {
    local $@;
    $evaled = eval $value;
    $error = $@;
  }
  if ($error) {
    die $error; # XXX
  }

  return $evaled;
} # eval_attr_value

1;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
