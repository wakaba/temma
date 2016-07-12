use strict;
use warnings;
use Path::Tiny;
use lib glob path (__FILE__)->parent->parent->child ('t_deps/modules/*/lib');
use AnyEvent;
use Test::X1;
use Test::More;
use Path::Class;
use Temma;

my $TemplatesD = file (__FILE__)->dir->subdir ('processor-2');

sub temma ($$$$) {
  my ($fh, $template_path, $args, $ok) = @_;
  Temma->process_html
      ($TemplatesD->file (@$template_path), $args => $fh,
       sub { undef $fh; AE::postpone { $ok->() } });
} # temma

{
  package test::Temma::Printer;
  
  sub new_from_value_ref ($$) {
    return bless {value_ref => $_[1]}, $_[0];
  } # new_from_value_ref

  sub print ($$) {
    ${$_[0]->{value_ref}} .= $_[1];
  } # print

  sub DESTROY {
    ${$_[0]->{value_ref}} .= '(end)';
  } # DESTROY
}

test {
  my $c = shift;
  my $v = '';
  temma +test::Temma::Printer->new_from_value_ref (\$v), ['1.html.tm'], {}, sub {
    test {
      is $v, q{<!DOCTYPE html><html><body>hoge</body></html>(end)};
    } $c;
    done $c;
    undef $c;
  };
} n => 1;

test {
  my $c = shift;
  my $v = '';
  temma +test::Temma::Printer->new_from_value_ref (\$v), ['2.html.tm'], {}, sub {
    test {
      is $v, q{<!DOCTYPE html><html><body>xyz</body></html>(end)};
    } $c;
    done $c;
    undef $c;
  };
} n => 1;

run_tests;
