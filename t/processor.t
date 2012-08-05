use strict;
use warnings;
no warnings 'redefine';
use Path::Class;
use lib file (__FILE__)->dir->parent->subdir ('lib')->stringify;
use lib glob file (__FILE__)->dir->parent->subdir ('modules', '*', 'lib')->stringify;
use Test::MoreMore;
use Test::HTCT::Parser;
use Temma::Parser;
use Temma::Processor;
use Message::DOM::DOMImplementation;
use Whatpm::HTML::Dumper;
use Temma::Defs;
use Test::X1;
use Encode;

my $test_data_d = file (__FILE__)->dir->subdir ('data')->subdir ('processing');

test {
  my $c = shift;
  my $text = q{
    <t:my as=$cv>
    <t:call x="
      use AnyEvent;
      $cv = AE::cv;
      my $old_time = time;
      my $timer; $timer = AE::timer 1, 0, sub {
        undef $timer;
        $cv->send (time - $old_time);
      };
    ">
    <p>Before wait</p>
    <t:wait cv=$cv as=$sleep>
    <p>After wait (delta = <t:text value=$sleep> seconds)
  };

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      like $result, qr{^<!DOCTYPE html><html><body><p>Before wait</p>
    
    <p>After wait \(delta = [1-9][0-9]* seconds\)</p></body></html>$};
      done $c;
    } $c;
  });
  like $result, qr{^<!DOCTYPE html><html><body><p>Before wait</p>$};
} n => 2, name => 'process_document ondone';

test {
  my $c = shift;
  my $dom = Message::DOM::DOMImplementation->new;

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
      $file_name = 'index.html.tm' unless defined $file_name;
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
    my $onerror = sub {
      my %opt = @_;
      my $node = $opt{node};
      $opt{f} ||= ($node->owner_document || $node)
          ->get_user_data ('manakai_source_f') if $node;
      while ($node) {
        $opt{line} //= $node->get_user_data ('manakai_source_line');
        $opt{column} //= $node->get_user_data ('manakai_source_column');
        last if defined $opt{line} and defined $opt{column};
        $node = $node->parent_node;
      }
      push @error,
          (($opt{f} && $opt{f} ne $initial_file_name) ? $opt{f} . ';' : '') .
          join ';', map { 
            defined $_ ? $_ : '';
          } @opt{qw(line column level type value text)};
    }; # onerror

    local *Temma::Processor::process_include = sub ($$%) {
      my ($self, $f, %args) = @_;

      my $file_name = $f->stringify;
      $file_name =~ s{/[^/]+/\.\.(?=/|$)}{}g;
      $f = file ($file_name);

      my $data = $data_by_file_name->{$file_name};
      unless ($data) {
        $args{onerror}->("File |$file_name| not found");
        return;
      }

      my $parser = Temma::Parser->new;
      my $doc = $args{dom}->create_document;
      $parser->{initial_state} = $args{parse_context};
      $parser->parse_char_string ($data->[0] => $doc, sub {
        $self->onerror->(@_, f => $f);
      });
      $doc->set_user_data (manakai_source_f => $f);
  
      $args{onparsed}->($doc);
    }; # process_include

    my $doc = $dom->create_document;
    my $data = $data_by_file_name->{$initial_file_name};
    $parser->parse_char_string ($data->[0] => $doc, $onerror);
    $doc->set_user_data (manakai_source_f => file ($initial_file_name));

    my $output = '';
    open my $fh, '>', \$output;
    binmode $fh, ':utf8';

    my $processor = Temma::Processor->new;
    $processor->onerror ($onerror);

    if ($test->{locale}) {
      my $package = 'test::Locale::' . int rand 1000000;
      eval ("package $package;\n" . $test->{locale}->[0] .
            "\nsub new { return bless {}, \$_[0] } 1;") or die $@;
      $processor->locale ($package->new);
    }

    $processor->process_document ($doc => $fh);

    while (@Test::Temma::CV::Instance) {
      my $cv = shift @Test::Temma::CV::Instance;
      $cv->send;
    }

    $output = decode 'utf8', $output;
    eq_or_diff $output, $test->{output}->[0];

    eq_or_diff [sort { $a cmp $b } @error],
        [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
  } for map { $test_data_d->file ($_) } qw(
    basic-1.dat
    basic-2.dat
    element-1.dat
    element-2.dat
    attr-1.dat
    attr-2.dat
    attr-3.dat
    comment-1.dat
    text-1.dat
    text-2.dat
    space-1.dat
    eval-1.dat
    wait-1.dat
    wait-2.dat
    if-1.dat
    call-1.dat
    for-1.dat
    for-2.dat
    for-3.dat
    try-1.dat
    try-2.dat
    try-3.dat
    try-4.dat
    my-1.dat
    barehtml-1.dat
    barehtml-2.dat
    macro-1.dat
    macro-2.dat
    macro-3.dat
    include-1.dat
    include-2.dat
    include-3.dat
    include-4.dat
    include-5.dat
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

test {
  my $c = shift;
  my $parser = Temma::Parser->new;
  my $f = $test_data_d->file ('doc-include-1.html.tm');
  my $doc = Message::DOM::DOMImplementation->new->create_document;
  $parser->parse_f ($f => $doc);

  my $output = '';
  open my $fh, '>', \$output;
  binmode $fh, ':utf8';

  my $processor = Temma::Processor->new;
  $processor->process_document ($doc => $fh);

  is $output, '<!DOCTYPE html><html><body><p>foo</p><div><p>abc</p><p>xyz</p></div>
<p>bar</p></body></html>';
  done $c;
} n => 1, name => 'include';

test {
  my $c = shift;
  my $parser = Temma::Parser->new;
  my $f = $test_data_d->file ('doc-include-3.html.tm');
  my $doc = Message::DOM::DOMImplementation->new->create_document;
  $parser->parse_f ($f => $doc);

  my $output = '';
  open my $fh, '>', \$output;
  binmode $fh, ':utf8';

  my @error;
  my $processor = Temma::Processor->new;
  $processor->onerror (sub {
    push @error, {@_};
  });
  $processor->process_document ($doc => $fh);

  is $output, '<!DOCTYPE html><html><body><p>foo</p><div';
  is $error[-1]->{type}, 'temma:include error';
  like $error[-1]->{value}, qr{not/found/file};
  done $c;
} n => 3, name => 'include';

test {
  my $c = shift;
  my $text = q{
    <html t:params=$hoge>
    <p><t:text value=$hoge>
  };

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => "hoge fuga\x{45000}",
  }, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<!DOCTYPE html><html><body><p>hoge fuga\x{45000}</p></body></html>};
      done $c;
    } $c;
  });
} n => 1, name => 'args required';

test {
  my $c = shift;
  my $text = q{
    <html t:params="$hoge $fuga?">
    <body>
    <t:if x=$fuga>
      <p>oops!
    <t:else>
      <p><t:text value="$hoge->{21}">
    </t:if>
  };

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => {21 => "hoge fuga\x{45000}"},
  }, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<!DOCTYPE html><html><body><p>hoge fuga\x{45000}</p></body></html>};
      done $c;
    } $c;
  });
} n => 1, name => 'args optional';

test {
  my $c = shift;
  my $text = q{
    <html t:params="$hoge">
    <p><t:text value="$hoge || 2">
  };

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => undef,
  }, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<!DOCTYPE html><html><body><p>2</p></body></html>};
      done $c;
    } $c;
  });
} n => 1, name => 'args undef';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->strict_error_checking (0);
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'fuga'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<hoge></hoge><fuga></fuga>};
      done $c;
    } $c;
  });
} n => 1, name => 'multiple root elements';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->strict_error_checking (0);
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));
  $doc->manakai_append_text ("ab c");
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'fuga'));
  $doc->manakai_append_text ("xz  ");

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<hoge></hoge><fuga></fuga>};
      done $c;
    } $c;
  });
} n => 1, name => 'text node children of document node';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><link><meta><p>abc<p>xyz</P>foo});
  $doc->strict_error_checking (0);
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->strict_error_checking (0);
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment no body';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><link><meta><p>abc<p>xyz</P>foo});
  my $attr = $doc->create_element_ns (TEMMA_NS, 't:attr');
  $attr->set_attribute (name => '"hoge"');
  $attr->set_attribute (value => 12);
  $doc->body->insert_before ($attr, $doc->body->first_child);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:attr';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment space';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });
  $doc->body->set_attribute_ns (TEMMA_NS, 't:space' => 'preserve');

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{  <p>abc</p><p>xyz</p>foo   };
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:space=preserve';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });
  $doc->document_element->set_attribute_ns (TEMMA_NS, 't:space' => 'preserve');

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{  <p>abc</p><p>xyz</p>foo   };
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:space=preserve root';

test {
  my $c = shift;

  my $dom = Message::DOM::DOMImplementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>ax});
  $doc->document_element->set_attribute_ns (TEMMA_NS, 't:params' => '$aa');
  my $text = $doc->create_element_ns (TEMMA_NS, 't:text');
  $text->set_attribute (value => '$aa');
  $doc->body->append_child ($text);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, args => {
    aa => "120 21",
  }, ondone => sub {
    test {
      $result = decode 'utf-8', $result;
      is $result, qq{ax120 21};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:params';

run_tests;

=head1 LICENSE

Copyright 2012 Wakaba <w@suika.fam.cx>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
