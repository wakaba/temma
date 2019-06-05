use strict;
use warnings;
use Web::Encoding;
use Web::DOM::Document;
use Temma::Parser;
use Temma::Compiler;
use Data::Dumper;
use WritableStream;

my $input = decode_web_utf8 (shift // '');

my $doc = new Web::DOM::Document;
my $parser = Temma::Parser->new;
$parser->parse_char_string ($input => $doc);

my $processor = Temma::Compiler->new;
$processor->compile ($doc)->then (sub {
  my $compiled = $_[0];
  warn Dumper $compiled;

  my $ws = WritableStream->new ({
    write => sub {
      print "|${$_[1]}|\n";
    },
    close => sub {
      print "close\n";
    },
  });
  return $processor->evaluate ($compiled => $ws);
})->to_cv->recv;
