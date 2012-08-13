use strict;
use warnings;
use Path::Class;
use lib file (__FILE__)->dir->parent->subdir ('lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('modules', '*', 'lib')->stringify;
use Test::MoreMore;
use Test::HTCT::Parser;
use Temma::Parser;
use Message::DOM::DOMImplementation;
use Whatpm::HTML::Dumper;
use Test::X1;

$Whatpm::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma>} = 'temma';
$Whatpm::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/macro>} = 'temmacro';
$Whatpm::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/msg>} = 'temsgid';
$Whatpm::HTML::Dumper::NamespaceMapping
    ->{q<http://suika.fam.cx/www/markup/temma/perl>} = 'templ';

my $test_data_d = file (__FILE__)->dir->subdir ('data')->subdir ('parsing');

test {
  my $c = shift;
  my $dom = Message::DOM::DOMImplementation->new;

  for_each_test $_->stringify, {
    data => {is_prefixed => 1},
    errors => {is_list => 1},
    document => {is_prefixed => 1},
  }, sub {
    my $test = shift;

    my @error;
    my $parser = Temma::Parser->new;
    my $onerror = sub {
      my %opt = @_;
      push @error, join ';', map { 
        defined $_ ? $_ : '';
      } @opt{qw(line column level type value text)};
    }; # onerror

    my $doc = $dom->create_document;

    $parser->parse_char_string ($test->{data}->[0] => $doc, $onerror);

    my $actual = dumptree $doc;
    $actual =~ s/\n$//;
    eq_or_diff $actual, $test->{document}->[0];

    eq_or_diff [sort { $a cmp $b } @error],
        [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
  } for map { $test_data_d->file ($_) } qw(
    cdata-1.dat
    cdata-2.dat
    rcdata-1.dat
    html-1.dat
    html-meta-1.dat
    html-void-1.dat
    html-flow-1.dat
    html-lists-1.dat
    html-tables-1.dat
    html-forms-1.dat
    html-text-1.dat
    svg-1.dat
    mml-1.dat
    temma-attr-1.dat
    temma-attr-2.dat
    temma-node-1.dat
    temma-node-2.dat
    temma-flow-1.dat
    temma-inclusion-1.dat
    temma-inclusion-2.dat
    temma-macro-1.dat
  );
  done $c;
};

test {
  my $c = shift;
  my $parser = Temma::Parser->new;
  my $f = $test_data_d->file ('doc-1.html.tm');
  my $doc = Message::DOM::DOMImplementation->new->create_document;
  $parser->parse_f ($f => $doc);
  is $doc->inner_html, q{<!DOCTYPE html><html>

  <head><link href="/foo/bar.css" rel="stylesheet">
  <title t:parse=""><t:text value=" foo () "></t:text></title>
  <t:macro name="text">
    <t:text value="bar()"></t:text>
  </t:macro>
  </head><body><p><m:text></m:text>

</p></body></html>};
  my $f2 = $doc->get_user_data ('manakai_source_f');
  isa_ok $f2, 'Path::Class::File';
  is $f2->stringify, $f->stringify;
  done $c;
} name => 'parse_f', n => 3;

run_tests;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
