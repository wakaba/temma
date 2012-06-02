package Temma::Processor;
use strict;
use warnings;
no warnings 'utf8';
our $VERSION = '1.0';
use Message::DOM::Node;
use Temma::Defs;

sub new {
  return bless {}, $_[0];
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
        my $ln = $node->manakai_local_name;
        print $fh '<' . $ln; # XXX

        for my $attr (@{$node->attributes}) {
          print $fh ' ' . $attr->node_name . '="' . # XXX
              (htescape $attr->node_value) . '"';
        }

        print $fh '>';

        my $ns = $node->namespace_uri || '';
        if ($ns eq HTML_NS and not $ln =~ /:/ and
            $Temma::Defs::Void->{$ln}) {
          #
        } else {
          unshift @process,
              {type => 'end tag', tag_name => $ln}; # XXX
          
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
    } else {
      die "Process type |$process->{type}| is not supported";
    }
  }

} # proess_document

1;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
