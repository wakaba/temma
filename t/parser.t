use strict;
use warnings;
use Path::Class;
use lib file (__FILE__)->dir->parent->subdir ('lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('modules', '*', 'lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('t_deps', 'modules', '*', 'lib')->stringify;
use Test::More;
use Test::Differences;
use Test::HTCT::Parser;
use Temma::Parser;
use Web::DOM::Document;
use Web::HTML::Dumper;
use Test::X1;

$Web::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma>} = 'temma';
$Web::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/macro>} = 'temmacro';
$Web::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/msg>} = 'temsgid';
$Web::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/perl>} = 'templ';

my $test_data_d = file (__FILE__)->dir->subdir ('data')->subdir ('parsing');

for (glob $test_data_d->file ('*.dat')) {
  my $file_name = $_;
  $file_name = $1 if m{([^/]+)$};
  for_each_test $_, {
    data => {is_prefixed => 1},
    errors => {is_list => 1},
    document => {is_prefixed => 1},
  }, sub {
    my $test = shift;

    test {
      my $c = shift;
      my @error;
      my $parser = Temma::Parser->new;
      my $onerror = sub {
        my %opt = @_;
        push @error, join ';', map { 
          defined $_ ? $_ : '';
        } @opt{qw(line column level type value text)};
      }; # onerror

      my $doc = new Web::DOM::Document;
      $parser->onerror ($onerror);
      $parser->parse_char_string ($test->{data}->[0] => $doc);

      my $actual = dumptree $doc;
      $actual =~ s/\n$//;
      eq_or_diff $actual, $test->{document}->[0];

      eq_or_diff [sort { $a cmp $b } @error],
          [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
      done $c;
    } n => 2, name => [$file_name, $test->{data}->[0]];
  };
}

test {
  my $c = shift;
  my $parser = Temma::Parser->new;
  my $f = $test_data_d->file ('doc-1.html.tm');
  my $doc = new Web::DOM::Document;
  $parser->parse_f ($f => $doc);
  is $doc->inner_html, q{<!DOCTYPE html><html>

  <head><link rel="stylesheet" href="/foo/bar.css">
  <title t:parse=""><t:text value=" foo () "></t:text></title>
  <t:macro name="text">
    <t:text value="bar()"></t:text>
  </t:macro>
  </head><body><p><m:text></m:text>

</p></body></html>};
  my $f2 = $doc->get_user_data ('manakai_source_f');
  isa_ok $f2, 'Path::Class::File';
  is $f2->stringify, $f->stringify;
  is $doc->get_user_data ('manakai_source_file_name'), $f2->stringify;
  done $c;
} name => 'parse_f', n => 4;

run_tests;

=head1 LICENSE

Copyright 2012-2015 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
