package Temma::Processor;
use strict;
use warnings;
no warnings 'utf8';
our $VERSION = '1.0';
use Message::DOM::Node;
use Temma::Defs;

sub IN_HTML () { 0 }
sub IN_SVG () { 1 }
sub IN_MML () { 2 }

sub new ($) {
  return bless {
    onerror => sub {
      my %args = @_;
      my $msg = $args{type};
      $msg .= ' (' . $args{value} . ')' if defined $args{value};
      if ($args{node}) {
        $msg .= ' at node ' . $args{node}->manakai_local_name;
        my $line = $args{node}->get_user_data ('manakai_source_line');
        my $column = $args{node}->get_user_data ('manakai_source_column');
        if ($line or $column) {
          $msg .= sprintf ' at line %d column %d',
              $line || 0, $column || 0;
        }
      }
      warn "$msg\n";
    },

    mode => IN_HTML,
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

  my @process;
  push @process, {type => 'node', node => $doc};

  while (@process) {
    my $process = shift @process;

    if ($process->{type} eq 'node') {
      my $node = $process->{node};
      my $nt = $node->node_type;
      if ($nt == TEXT_NODE) {
        print $fh htescape $node->data;
      } elsif ($nt == ELEMENT_NODE) {
        my $ns = $node->namespace_uri || '';
        my $ln = $node->manakai_local_name;
        my $attrs = [];
        if ($ns eq TEMMA_NS) {
          if ($ln eq 'element') {
            $ln = $self->eval_attr_value ($node, 'name'); # XXX
            $ln = '' unless defined $ln;
            $ln =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
            $ns = $node->$TemmaContextNode->manakai_get_child_namespace_uri ($ln);
          } else {
            $self->{onerror}->(type => 'temma:unknown element',
                               node => $node,
                               value => $ln,
                               level => 'm');
            next;
          }
        } else {
          $attrs = $node->attributes;
        }

        my $current_mode = $self->{mode};

          print $fh '<' . $ln; # XXX
          
          for my $attr (@$attrs) {
            print $fh ' ' . $attr->node_name . '="' . # XXX
                (htescape $attr->node_value) . '"';
          }
          
          print $fh '>';
          
          if ($ns eq HTML_NS and not $ln =~ /:/ and
              $Temma::Defs::Void->{$ln}) {
            #
        } else {
          if ($current_mode != $self->{mode}) {
            unshift @process,
                {type => 'end tag', tag_name => $ln, # XXX
                 mode => $current_mode};
          } else {
            unshift @process,
                {type => 'end tag', tag_name => $ln}; # XXX
          }
            
            unshift @process,
                map { {type => 'node', node => $_} } 
                grep { $_->node_type == ELEMENT_NODE or
                       $_->node_type == TEXT_NODE }
                @{$node->child_nodes->to_a};
          }
      } elsif ($nt == DOCUMENT_TYPE_NODE) {
        my $nn = $node->node_name;
        $nn =~ s/[^0-9A-Za-z_-]/_/g;
        print $fh '<!DOCTYPE ' . $nn . '>';
      } elsif ($nt == DOCUMENT_NODE) {
        unshift @process,
            map { {type => 'node', node => $_} } 
            grep { $_->node_type == ELEMENT_NODE or
                   $_->node_type == DOCUMENT_TYPE_NODE }
            @{$node->child_nodes->to_a};
      } else {
        die "Unknown node type |$nt|";
      }

    } elsif ($process->{type} eq 'end tag') {
      print $fh '</' . $process->{tag_name} . '>';
      if ($process->{mode}) {
        $self->{mode} = $process->{mode};
      }
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  }

} # proess_document

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
