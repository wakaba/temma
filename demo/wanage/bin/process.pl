use strict;
use warnings;
use Path::Class;
use lib glob file (__FILE__)->dir->parent->subdir ('local', 'submodules', '*', 'lib')->stringify;
use Wanage::HTTP;
use Temma;

{
  package App::Web;
  use Path::Class;

  my $templates_d = file(__FILE__)->dir->parent->subdir('templates');

  sub process_temma ($$$%) {
    my ($class, $http, $template_path, $args) = @_;
    $http->response_mime_type->set_value ('text/html');
    $http->response_mime_type->set_param (charset => 'utf-8');
    my $fh = App::Web::Printer->new_from_http ($http);
    Temma->process_html ($templates_d->file(@$template_path), $args => $fh,
                         sub { $http->close_response_body });
  } # process_temma
}

{
  package App::Web::Printer;
  
  sub new_from_http ($$) {
    return bless {http => $_[1], s => []}, $_[0];
  } # new_from_http
  
  sub print ($$) {
    if (1) {
      if (length $_[1]) {
        push @{$_[0]->{s}}, $_[1];
      } else {
        $_[0]->{http}->send_response_body_as_text (join '', @{$_[0]->{s}});
        $_[0]->{s} = [];
      }
    } else {
      $_[0]->{http}->send_response_body_as_text ($_[1]);
    }
  } # print
}

my $http = do {
  Wanage::HTTP->new_cgi;
};

App::Web->process_temma ($http, ['test1.html.tm']);
