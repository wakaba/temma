package Temma;
use strict;
use warnings;
our $VERSION = '1.0';
use Web::DOM::Document;
use Temma::Parser;
use Temma::Processor;

our $Locale;

sub process_html ($$$$$) {
  #my ($class, $input, $args => $fh, $cb) = @_;
  my $doc = new Web::DOM::Document;
  my $parser = Temma::Parser->new;
  if (UNIVERSAL::isa ($_[1], 'Path::Class::File')) {
    $parser->parse_f ($_[1] => $doc);
  } else {
    $parser->parse_char_string ($_[1] => $doc);
  }
  my $processor = Temma::Processor->new;
  $processor->locale ($Locale);
  $processor->process_document ($doc => $_[3], ondone => $_[4], args => $_[2]);
} # process_html

sub process_html_fragment ($$$$$) {
  #my ($class, $input, $args => $fh, $cb) = @_;
  my $doc = new Web::DOM::Document;
  my $parser = Temma::Parser->new;
  if (UNIVERSAL::isa ($_[1], 'Path::Class::File')) {
    $parser->parse_f ($_[1] => $doc);
  } else {
    $parser->parse_char_string ($_[1] => $doc);
  }
  my $processor = Temma::Processor->new;
  $processor->locale ($Locale);
  $processor->process_fragment ($doc => $_[3], ondone => $_[4], args => $_[2]);
} # process_html_fragment

sub process_plain_text ($$$$$) {
  #my ($class, $input, $args => $fh, $cb) = @_;
  my $doc = new Web::DOM::Document;
  my $parser = Temma::Parser->new;
  if (UNIVERSAL::isa ($_[1], 'Path::Class::File')) {
    $parser->parse_f ($_[1] => $doc);
  } else {
    $parser->parse_char_string ($_[1] => $doc);
  }
  my $processor = Temma::Processor->new;
  $processor->locale ($Locale);
  $processor->process_plain_text ($doc => $_[3], ondone => $_[4], args => $_[2]);
} # process_plain_text

1;

=head1 LICENSE

Copyright 2012-2013 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
