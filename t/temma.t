use strict;
BEGIN {
  my $file_name = __FILE__; $file_name =~ s{[^/\\]+$}{}; $file_name ||= '.';
  $file_name .= '/../config/perl/libs.txt';
  if (-f $file_name) {
    open my $file, '<', $file_name or die "$0: $file_name: $!";
    unshift @INC, split /:/, <$file>;
  }
}
use warnings;
use Test::X1;
use Test::More;
use Temma;
use Path::Class;
use File::Temp qw(tempfile);

{
  package test::fh;
  sub print {
    ${$_[0]} .= $_[1];
  }
}

sub create_file ($) {
  my ($fh, $file_name) = tempfile;
  binmode $fh, q(:utf8);
  print $fh $_[0];
  close $fh;
  return file ($file_name);
} # create_file

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt; &};
  my $output = '';
  my $fh = bless \$output, 'test::fh';

  Temma->process_html ($input => $fh, sub {
    test {
      is $output, qq{<!DOCTYPE html><html><head><link></head><body>123\x{4e00}&lt; &amp;</body></html>};
      done $c;
    } $c;
  });
} n => 1, name => ['process_html', 'input=string'];

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt;&};
  my $output = '';
  my $fh = bless \$output, 'test::fh';
  my $f = create_file $input;

  Temma->process_html ($f => $fh, sub {
    test {
      is $output, qq{<!DOCTYPE html><html><head><link></head><body>123\x{4e00}&lt;&amp;</body></html>};
      done $c;
    } $c;
  });
} n => 1, name => ['process_html', 'input=file'];

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt;&};
  my $output = '';
  my $fh = bless \$output, 'test::fh';

  Temma->process_html_fragment ($input => $fh, sub {
    test {
      is $output, qq{123\x{4e00}&lt;&amp;};
      done $c;
    } $c;
  });
} n => 1, name => ['process_html_fragment', 'input=string'];

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt;&};
  my $output = '';
  my $fh = bless \$output, 'test::fh';
  my $f = create_file $input;

  Temma->process_html_fragment ($f => $fh, sub {
    test {
      is $output, qq{123\x{4e00}&lt;&amp;};
      done $c;
    } $c;
  });
} n => 1, name => ['process_html_fragment', 'input=file'];

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt;&};
  my $output = '';
  my $fh = bless \$output, 'test::fh';

  Temma->process_plain_text ($input => $fh, sub {
    test {
      is $output, qq{123\x{4e00}<&};
      done $c;
    } $c;
  });
} n => 1, name => ['process_plain_text', 'input=string'];

test {
  my $c = shift;

  my $input = qq{<link><body><t:for x=[1,2,3] as=v><t:text value=\$v></t:for>\x{4e00}&lt;&};
  my $output = '';
  my $fh = bless \$output, 'test::fh';
  my $f = create_file $input;

  Temma->process_plain_text ($f => $fh, sub {
    test {
      is $output, qq{123\x{4e00}<&};
      done $c;
    } $c;
  });
} n => 1, name => ['process_plain_text', 'input=file'];

run_tests;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
