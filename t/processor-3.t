use strict;
use warnings;
use Path::Tiny;
use lib glob path (__FILE__)->parent->parent->child ('t_deps/modules/*/lib');
use Test::X1;
use Test::More;
use Web::DOM::Implementation;
use Temma::Parser;
use Temma::Processor;
use Web::Encoding;

test {
  my $c = shift;
  my $text = q{
    <html>
    <t:my as=$x x="
      bless {}, 'test::package1';
    ">
    <t:text value="$x?1:0">
  };

  $test::package1::destroyed = 0;
  {
    package test::package1;
    sub DESTROY {
      $test::package1::destroyed++;
    }
  }

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body>1</body></html>};
      is $test::package1::destroyed, 1;
      done $c;
    } $c;
  });
} n => 2, name => 'variable t:my';

test {
  my $c = shift;
  my $text = q{
    <html>
    <t:my as=$x x="
      our $X;
      $X = bless {}, 'test::package2';
    ">
    <t:text value="$x?1:0">
  };

  $test::package2::destroyed = 0;
  {
    package test::package2;
    sub DESTROY {
      $test::package2::destroyed++;
    }
  }

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body>1</body></html>};
      is $test::package2::destroyed, 1;
      done $c;
    } $c;
  });
} n => 2, name => 'variable our';

test {
  my $c = shift;
  my $text = q{
    <html>
    <t:my as=$x x="
      my $x = bless {}, 'test::package3';
      sub abc { $x }
    ">
    <t:text value="$x?1:0">
  };

  $test::package3::destroyed = 0;
  {
    package test::package3;
    sub DESTROY {
      $test::package3::destroyed++;
    }
  }

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body>1</body></html>};
      is $test::package3::destroyed, 1;
      done $c;
    } $c;
  });
} n => 2, name => 'subroutine';

test {
  my $c = shift;
  my $text = q{
    <html>
    <t:my as=$x x="
      $Global::X = bless {}, 'test::package2';
    ">
    <t:text value="$x?1:0">
  };

  $test::package4::destroyed = 0;
  {
    package test::package4;
    sub DESTROY {
      $test::package4::destroyed++;
    }
  }

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body>1</body></html>};
      is $test::package4::destroyed, 0;
      done $c;
    } $c;
  });
} n => 2, name => 'variable global';

test {
  my $c = shift;
  my $text = q{
    <html>
    <t:text value=3>
    <t:my as=$x x='
      die \"abc";
    '>
    <t:text value=4>
  };

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  my @error;
  $pro->onerror (sub {
    my %args = @_;
    push @error, $args{type};
  });
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body>3</body></html>};
      is 0+@error, 1;
      is $error[0], 'temma:perl exception';
      done $c;
    } $c;
  });
} n => 3, name => 'exception';

run_tests;
