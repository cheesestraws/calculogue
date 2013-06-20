package modules::Foo;
use strict;
use TISBLModule;
use base 'TISBLModule';

verb { "foo" } is {
	print "Hello world from perl-land!\n";
};




1;