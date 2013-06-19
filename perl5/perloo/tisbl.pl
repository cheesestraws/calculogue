#!/usr/bin/env perl
use strict;
use InterpState;
use Stdlib;
use Parser qw(parseline);
use SI;

sub repl {
	my $line;
	my $tokens;

	# create an interpreter state
	my $state = InterpState->new;
	Stdlib->install($state->verbs);
	$state->start(undef, undef);
	print "TISBL-perloo 1.0\n";
	print "> ";
	while ($line = <STDIN>) {
		$tokens = parseline($line);
		
		# and sneak each onto the execution stack
		for my $tok (@$tokens) {
			$state->current->exec->sneak(SI::Str($tok));
		}
		eval {
			$state->current->go;
		};
		if ($@) {
			print "[ERROR] $@ \n";
		}
	
		# print next prompt
		print "> ";
	}
	print "\n";
}

sub loadfile {
	my $fn = shift;
	my $line;
	my $tokens;

	# create an interpreter state
	my $state = InterpState->new;
	Stdlib->install($state->verbs);
	$state->start(undef, undef);
	
	# open the file
	open my $fh, '<', $fn;
	
	while ($line = <$fh>) {
		$line =~ tr/\x{d}\x{a}//d;
		$tokens = parseline($line);
		
		# and sneak each onto the execution stack
		for my $tok (@$tokens) {
			$state->current->exec->sneak(SI::Str($tok));
		}
	}

	close $fh;
	$state->current->go;
	$state->stop;
}


sub help {
print q{Usage:
  tisbl.pl -r              Start a REPL
  tisbl.pl -f [filename]   Run filename
};
}



# check command line parameters
if ($ARGV[0] eq '-r') {
	repl;
} elsif ($ARGV[0] eq '-f') {
	loadfile($ARGV[1]);
} else {
	help;
}


1;