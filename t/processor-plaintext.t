use strict;
use warnings;
no warnings 'redefine';
use Path::Class;
use lib file (__FILE__)->dir->parent->subdir ('lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('modules', '*', 'lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('t_deps', 'modules', '*', 'lib')->stringify;
use Test::More;
use Test::Differences;
use Test::HTCT::Parser;
use Temma::Parser;
use Temma::Processor;
use Web::Encoding qw(decode_web_utf8);
use Web::DOM::Document;
use Web::HTML::Dumper;
use Web::HTML::SourceMap;
use Temma::Defs;
use Test::X1;

my $test_data_d = file (__FILE__)->dir->subdir ('data')->subdir ('processing');

test {
  my $c = shift;

  for_each_test $_->stringify, {
    data => {is_prefixed => 1, multiple => 1},
    errors => {is_list => 1},
    output => {is_prefixed => 1},
  }, sub {
    my $test = shift;

    my $datas = $test->{data} || [];
    my $data_by_file_name = {};
    my $initial_file_name;
    for my $data (@$datas) {
      my $file_name = $data->[1]->[-1];
      $file_name = 'index.txt.tm' unless defined $file_name;
      $file_name = '/' . $file_name unless $file_name =~ m{^/};
      if ($data_by_file_name->{$file_name}) {
        die "Multiple #data section with file name |$file_name|";
        next;
      }
      $data_by_file_name->{$file_name} = $data;
      $initial_file_name ||= $file_name;
    }

    my @error;
    my $parser = Temma::Parser->new;
    my $dids = $parser->di_data_set;
    my $onerror = sub {
      my %opt = @_;
      my $node = $opt{node};
      $opt{f} ||= ($node->owner_document || $node)
          ->get_user_data ('manakai_source_f') if $node;
      while ($node) {
        my $sl = $node->manakai_get_source_location;
        unless ($sl->[1] == -1) {
          $opt{di} = $sl->[1];
          $opt{index} = $sl->[2];
          last;
        }
        $node = $node->parent_node;
      }
      ($opt{line}, $opt{column}) = index_pair_to_lc_pair
          $dids, $opt{di}, $opt{index};

      push @error,
          (($opt{f} && $opt{f} ne $initial_file_name) ? $opt{f} . ';' : '') .
          join ';', map { 
            defined $_ ? $_ : '';
          } @opt{qw(line column level type value text)};
    }; # onerror

    my $oninclude = sub {
      my $x = $_[0];

      my $file_name = $x->{f}->stringify;
      $file_name =~ s{/[^/]+/\.\.(?=/|$)}{}g;
      $x->{f} = file ($file_name);

      my $data = $data_by_file_name->{$file_name};
      die "File |$file_name| not found" unless $data;

      my $parser = $x->{get_parser}->();
      my $doc = $x->{create_document}->();

      $parser->parse_char_string ($data->[0] => $doc);
      $doc->set_user_data (manakai_source_f => $x->{f});
      $doc->set_user_data (manakai_source_file_name => $x->{f}->stringify);

      return $doc;
    }; # oninclude

    my $doc = Web::DOM::Document->new;
    my $data = $data_by_file_name->{$initial_file_name};
    $parser->{initial_state} = 'body'
        unless ($data->[1]->[-1] || '') eq 'html';

    $parser->onerror ($onerror);
    $parser->parse_char_string ($data->[0] => $doc);
    $doc->set_user_data (manakai_source_f => file ($initial_file_name));
    $doc->set_user_data (manakai_source_file_name => $initial_file_name);

    my $output = '';
    open my $fh, '>', \$output;
    binmode $fh, ':utf8';

    my $processor = Temma::Processor->new;
    $processor->onerror ($onerror);
    $processor->oninclude ($oninclude);
    $processor->di_data_set ($dids);

    if ($test->{locale}) {
      my $package = 'test::Locale::' . int rand 1000000;
      eval ("package $package;\n" . $test->{locale}->[0] .
            "\nsub new { return bless {}, \$_[0] } 1;") or die $@;
      $processor->locale ($package->new);
    }

    $processor->process_plain_text ($doc => $fh);

    while (@Test::Temma::CV::Instance) {
      my $cv = shift @Test::Temma::CV::Instance;
      $cv->send;
    }

    $output = decode_web_utf8 $output;
    eq_or_diff $output, $test->{output}->[0];

    eq_or_diff [sort { $a cmp $b } @error],
        [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
  } for map { $test_data_d->file ($_) } qw(
    plaintext-basic-1.dat
    plaintext-basic-2.dat
    plaintext-flow-1.dat
    plaintext-text-1.dat
    plaintext-eval-1.dat
    plaintext-macro-1.dat
    plaintext-msgid-1.dat
    plaintext-html-template-1.dat
  );
  $c->done;
};

{
  package Test::Temma::CV;

  our @Instance;
  
  sub new ($) {
    my $class = shift;
    my $self = bless {@_}, $class;
    push @Instance, $self;
    return $self;
  } # new

  sub cb ($;$) {
    if (@_ > 1) {
      $_[0]->{cb} = $_[1];
    }
    return $_[0]->{cb};
  } # cb

  sub send ($;$) {
    $_[0]->{sent_value} = $_[1] if exists $_[1];
    $_[0]->{cb}->($_[0]);
  } # send

  sub recv ($) {
    return $_[0]->{sent_value};
  } # recv
}

run_tests;

=head1 LICENSE

Copyright 2012-2015 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
