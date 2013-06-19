package Parser;
use strict;
use base 'Exporter';
our @EXPORT_OK = qw(parseline);

sub parseline {
	my $line = shift;
	
	# kill any comments on this line
	$line =~ s/\s+%.*$//;
	$line =~ s/^%.*$//;

	
	my @elems = split /\s+/, $line;
	return \@elems;
}


1;