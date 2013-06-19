package TISBLModule;
use strict;
use base 'Exporter';
our @EXPORT = qw(verb is state ctx input output verbsub);

# This package contains utilities for making TISBL libraries.
# Specifically, the stdlib.

# A library is effectively a set of name->code mappings.
# The code, in this case, will be a perl sub ref.
# Each library's name->code mappings need to be kept
# separate.  Each library needs to provide an
# install method to install the library in an interpreter.
#
# In this case, we'll make each library correspond to a
# Perl package, so that libraries can be written just as
# standard perl classes, and inherit install() etc from
# this class.
#
# This package uses a variant on the inside-out object idea
# only indexed on package names - specifically, this 'ere
# hash will contain one name => value pair per package,
# and the value will be a hashref containing name => value
# mappings where the name is a verbname and the value
# is a coderef.
#
# There needs to be more documentation here!
my %verbs = ();


# This is a scratch variable name to pass parameters with
our %state;


# See the try/catch example in perlsub for how this works
sub verb(&@) {
	my ($nameproc, $do) = @_;
	
	# get the name of the package that called us
	my ($package, $filename, $line) = caller;
	
	# create an entry in our verbs hash for that 
	$verbs{$package} = {} unless exists $verbs{$package};
	
	my $name = $nameproc->();
	my $proc = sub {
		my ($state, $ctx, $input, $output) = @_;
		local %state;
		$state{'state'} = $state;
		$state{'ctx'} = $ctx;
		$state{'input'} = $input;
		$state{'output'} = $output;
		$do->();
	};
	$verbs{$package}->{$name} = $proc;
}

sub verbsub(&) {
	my $vp = shift;
	my $proc = sub {
		my ($state, $ctx, $input, $output) = @_;
		local %state;
		$state{'state'} = $state;
		$state{'ctx'} = $ctx;
		$state{'input'} = $input;
		$state{'output'} = $output;
		$vp->();
	};
	return $proc;
}

sub is(&) {
	return $_[0];
}

sub verbs {
	my ($class) = @_;
	return $verbs{$class};
}

sub install {
	my ($class, $hashref) = @_;
	my $verbtable = $verbs{$class};
	for my $k (keys %$verbtable) {
		$hashref->{$k} = $verbtable->{$k};
	}
}

sub installOneVerb {
	my ($class, $hashref, $name) = @_;
	my $verbtable = $verbs{$class};
	$hashref->{$name} = $verbtable->{$name};
}

sub state() {
	return $state{'state'};
}

sub ctx() {
	return $state{'ctx'};
}

sub input() {
	return $state{'input'};
}

sub output() {
	return $state{'output'};
}


1;