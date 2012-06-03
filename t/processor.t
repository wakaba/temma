package test::Temma::Processor;
use strict;
use warnings;
use Path::Class;
use lib file (__FILE__)->dir->parent->subdir ('lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('modules', '*', 'lib')->stringify;
use Test::MoreMore;
use Test::HTCT::Parser;
use Temma::Parser;
use Temma::Processor;
use Message::DOM::DOMImplementation;
use Whatpm::HTML::Dumper;
use base qw(Test::Class);
use Encode;

my $test_data_d = file (__FILE__)->dir->subdir ('data')->subdir ('processing');

sub _processed : Tests {
  my $dom = Message::DOM::DOMImplementation->new;

  for_each_test $_->stringify, {
    data => {is_prefixed => 1},
    errors => {is_list => 1},
    output => {is_prefixed => 1},
  }, sub {
    my $test = shift;

    my @error;
    my $parser = Temma::Parser->new;
    my $onerror = sub {
      my %opt = @_;
      if ($opt{node}) {
        $opt{line} //= $opt{node}->get_user_data ('manakai_source_line');
        $opt{column} //= $opt{node}->get_user_data ('manakai_source_column');
      }
      push @error, join ';', map { 
        defined $_ ? $_ : '';
      } @opt{qw(line column level type value text)};
    }; # onerror

    my $doc = $dom->create_document;
    $parser->parse_char_string ($test->{data}->[0] => $doc, $onerror);

    my $output = '';
    open my $fh, '>', \$output;
    binmode $fh, ':utf8';

    my $processor = Temma::Processor->new;
    $processor->onerror ($onerror);
    $processor->process_document ($doc => $fh);

    $output = decode 'utf8', $output;
    eq_or_diff $output, $test->{output}->[0];

    eq_or_diff [sort { $a cmp $b } @error],
        [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
  } for map { $test_data_d->file ($_) } qw(
    basic-1.dat
    element-1.dat
  );
} # _processed

__PACKAGE__->runtests;

1;

