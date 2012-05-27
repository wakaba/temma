package test::Temma::Parser;
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
use base qw(Test::Class);

my $test_data_d = file (__FILE__)->dir->subdir ('data');

sub _parsed_tree : Tests {
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
        [sort { $a cmp $b } @{$test->{errors}->[0]}];
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
  );
} # _parsed_tree

__PACKAGE__->runtests;

1;

