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
use Temma::Compiler;
use Web::Encoding;
use Web::DOM::Implementation;
use Web::DOM::Document;
use Web::HTML::Dumper;
use Web::HTML::SourceMap;
use Temma::Defs;
use Test::X1;
use WritableStream;

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

  my $dom = Web::DOM::Implementation->new;
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

for (glob $test_data_d->file ('*.dat')) {
  my $file_name = $_;
  $file_name = $1 if m{([^/]+)$};
  next if $file_name =~ /plaintext/;

  for_each_test $_, {
    data => {is_prefixed => 1, multiple => 1},
    errors => {is_list => 1},
    output => {is_prefixed => 1},
  }, sub {
    my $test = shift;
    test {
      my $c = shift;
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

      my $doc_to_file_name = {};

      my @error;
      my $parser = Temma::Parser->new;
      my $dids = $parser->di_data_set;
      my $onerror = sub {
        my %opt = @_;
        my $node = $opt{node};
        $opt{file_name} ||= $doc_to_file_name->{$node->owner_document || $node}
            if defined $node;
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

        if (defined $opt{value}) {
          ## Error messages depend on Perl version...
          $opt{value} =~ s{ \(did you forget to declare "my [^"]+"\?\)}{}g;
          $opt{value} =~ s{ at .*?processor.t line \d+\.?\s*$}{}g;
        }

        push @error,
            (defined $opt{file_name} && $opt{file_name} ne $initial_file_name ? $opt{file_name} . ';' : '') .
            join ';', map { 
              defined $_ ? $_ : '';
            } @opt{qw(line column level type value text)};
      }; # onerror

      my $oninclude = sub {
        my $x = $_[0];

        my $base_f = file ($doc_to_file_name->{$x->{context}->owner_document});
        my $included_f = file ($x->{path});
        $included_f = $included_f->absolute ($base_f->dir) if $base_f;

        my $file_name = $included_f->stringify;
        $file_name =~ s{/[^/]+/\.\.(?=/|$)}{}g;

        my $data = $data_by_file_name->{$file_name};
        die "File |$file_name| not found" unless $data;

        my $parser = $x->{get_parser}->();
        $parser->onerror (sub {
          $x->{onerror}->(@_, file_name => $file_name);
        });

        my $doc = new Web::DOM::Document;
        $parser->parse_char_string ($data->[0] => $doc);
        $doc_to_file_name->{$doc} = $file_name;
        $x->{doc_to_path}->{$doc} = $file_name;

        return $doc;
      }; # oninclude

      my $doc = new Web::DOM::Document;
      my $data = $data_by_file_name->{$initial_file_name};
      $parser->onerror ($onerror);
      $parser->parse_char_string ($data->[0] => $doc);
      $doc->set_user_data (manakai_source_f => file ($initial_file_name));
      $doc_to_file_name->{$doc} = $initial_file_name;

      my $output = '';
      open my $fh, '>', \$output;
      binmode $fh, ':utf8';

      my $processor = Temma::Compiler->new;
      $processor->onerror ($onerror);
      $processor->oninclude ($oninclude);
      $processor->di_data_set ($dids);

      my $result = '';
      my $closed;
      $processor->compile ($doc)->then (sub {
        my $compiled = $_[0];
        #warn Dumper $compiled;

        my $ws = WritableStream->new ({
          write => sub {
            $result .= ${$_[1]};
          },
          close => sub {
            $closed = 1;
          },
        });
        return $processor->evaluate ($compiled => $ws);
      })->catch (sub {
        my $e = $_[0];
        test {
          ok 0; # XXX
          is $e, undef;
        } $c;
      })->then (sub {
        test {
          ok $closed;
          $result = decode_web_utf8 encode_web_utf8 $result;
          eq_or_diff $result, $test->{output}->[0];

          eq_or_diff [sort { $a cmp $b } @error],
              [sort { $a cmp $b } @{$test->{errors}->[0] or []}];
        } $c;
      })->then (sub {
        done $c;
        undef $c;
      });
    } n => 3, name => ['process_document', $file_name, $test->{data}->[0]->[0]];
  };
}

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
  my $doc = Web::DOM::Implementation->new->create_document;
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
  my $doc = Web::DOM::Implementation->new->create_document;
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

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => "hoge fuga\x{45000}",
  }, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
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

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => {21 => "hoge fuga\x{45000}"},
  }, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
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

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, args => {
    hoge => undef,
  }, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body><p>2</p></body></html>};
      done $c;
    } $c;
  });
} n => 1, name => 'args undef';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->dom_config->{manakai_strict_document_children} = -0;
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'fuga'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<hoge></hoge><fuga></fuga>};
      done $c;
    } $c;
  });
} n => 1, name => 'multiple root elements';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->dom_config->{manakai_strict_document_children} = -0;
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));
  $doc->manakai_append_text ("ab c");
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'fuga'));
  $doc->manakai_append_text ("xz  ");

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<hoge></hoge><fuga></fuga>};
      done $c;
    } $c;
  });
} n => 1, name => 'text node children of document node';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><link><meta><p>abc<p>xyz</P>foo});
  $doc->dom_config->{manakai_strict_document_children} = -0;
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->strict_error_checking (0);
  $doc->append_child ($doc->create_element_ns (HTML_NS, 'hoge'));

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment no body';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
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
      $result = decode_web_utf8 $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:attr';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<p>abc</p><p>xyz</p>foo};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment space';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });
  $doc->body->set_attribute_ns (TEMMA_NS, 't:space' => 'preserve');

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{  <p>abc</p><p>xyz</p>foo   };
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:space=preserve';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;
  $doc->manakai_is_html (1);
  $doc->inner_html (q{<!DOCTYPE html><body>  <p>abc<p>xyz</P>foo   });
  $doc->document_element->set_attribute_ns (TEMMA_NS, 't:space' => 'preserve');

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_fragment ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{  <p>abc</p><p>xyz</p>foo   };
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:space=preserve root';

test {
  my $c = shift;

  my $dom = Web::DOM::Implementation->new;
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
      $result = decode_web_utf8 $result;
      is $result, qq{ax120 21};
      done $c;
    } $c;
  });
} n => 1, name => 'process_fragment t:params';

{
  package test::pack1;
  my $destroyed = 0;
  sub DESTROY {
    $destroyed++;
  }
  sub destroyed { return $destroyed }
}
test {
  my $c = shift;

  my $text = q{
    <html>
    <body>
      <t:call x=" our $Hoge = bless [123], 'test::pack1' ">
      <p><t:text value="(our $Hoge)->[0]"></p>
  };

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body><p>123</p></body></html>};
      is +test::pack1->destroyed, 1;
      done $c;
      undef $c;
    } $c;
  });
} n => 2, name => 'global variables';

{
  package test::pack1_2;
  my $destroyed = 0;
  sub DESTROY {
    $destroyed++;
  }
  sub destroyed { return $destroyed }
}
test {
  my $c = shift;

  my $text = q{
    <html>
    <body>
      <t:call x=" our $Hoge = bless [123], 'test::pack1_2' ">
      <p><t:text value="(our $Hoge)->[0]"></p>
      <t:call x=die>xyz
  };

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body><p>123</p></body></html>};
      is +test::pack1_2->destroyed, 1;
      done $c;
      undef $c;
    } $c;
  });
} n => 2, name => 'global variables';

{
  package test::pack2;
  my $destroyed = 0;
  sub DESTROY {
    $destroyed++;
  }
  sub destroyed { return $destroyed }
}
test {
  my $c = shift;

  my $text = q{
    <html>
    <body>
      <t:my as=$Hoge x=" bless [123], 'test::pack2' ">
      <p><t:text value="$Hoge->[0]"></p>
  };

  my $dom = Web::DOM::Implementation->new;
  my $doc = $dom->create_document;

  my $parser = Temma::Parser->new;
  $parser->parse_char_string ($text => $doc);

  my $pro = Temma::Processor->new;
  open my $file, '>:utf8', \(my $result = '');
  $pro->process_document ($doc => $file, ondone => sub {
    test {
      $result = decode_web_utf8 $result;
      is $result, qq{<!DOCTYPE html><html><body><p>123</p></body></html>};
      is +test::pack2->destroyed, 1;
      done $c;
      undef $c;
    } $c;
  });
} n => 2, name => 't:my variables';

run_tests;

=head1 LICENSE

Copyright 2012-2016 Wakaba <wakaba@suikawiki.org>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
