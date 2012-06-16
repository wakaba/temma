package Temma::Exception;
use strict;
use warnings;
our $VERSION = '1.0';
use overload '""' => 'stringify', fallback => 1;

sub new_from_value ($$) {
  return bless {value => $_[1]}, $_[0];
} # new_from_exception

sub source_text ($;$) {
  if (@_ > 1) {
    $_[0]->{source_text} = $_[1];
  }
  return $_[0]->{source_text};
} # source_text

sub source_node ($;$) {
  if (@_ > 1) {
    $_[0]->{source_node} = $_[1];
  }
  return $_[0]->{source_node};
} # source_node

sub stringify ($) {
  return $_[0]->{value};
} # stringify

1;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
