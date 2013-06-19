package test::TISBLModule;
use strict;

use TISBLModule qw/!is/;
use Stack;

use Test::More tests => 16;
use base 'TISBLModule';

# create two verbs and check they go to the right place
verb { "v1" } TISBLModule::is {
	;
};
verb { "v2" } TISBLModule::is {
	;
};


my $vt = test::TISBLModule->verbs;
is(ref($vt), "HASH", "Verbtable is a hash.");
is(scalar keys %$vt, 2, "Verbtable contains two verbs");

# is it separating different packages?
package test::TISBLModule::subpackage;
use strict;
use TISBLModule;
use base 'TISBLModule';

verb { "v3" } is {
	;
};

package test::TISBLModule;

is(scalar keys %$vt, 2, "Adding verb in different package doesn't alter this vt");


# Are our parameters being passed through correctly?
my $state = 1;
my $ctx = 2;
my $input = 3;
my $output = 4;

verb { "v4" } TISBLModule::is {
	my $st = state;
	my $ct = ctx;
	my $inp = input;
	my $outp = output;
	
	is ($st, 1, "State value appearing correctly in verb");
	is ($ct, 2, "Context value appearing correctly in verb");
	is ($inp, 3, "Input value appearing correctly in verb");
	is ($outp, 4, "Output value appearing correctly in verb");
};

$vt->{'v4'}->($state, $ctx, $input, $output);


# Even if verb calls are nested?
verb { "v6" } TISBLModule::is {
	is(state, "how", "State being passed to inner verb correctly");
	is(ctx, "ghosts", "Context being passed to inner verb correctly");
	is(input, "affect", "Input being passed to inner verb correctly");
	is(output, "relationships", "Output being passed to inner verb correctly");
};

verb { "v7" } TISBLModule::is {
	$vt->{'v6'}->("how","ghosts","affect","relationships");
	
	is(state, "knees", "State being preserved in outer verb correctly");
	is(ctx, "up", "Context being preserved in outer verb correctly");
	is(input, "mother", "Input being preserved in outer verb correctly");
	is(output, "brown", "Output being preserved in outer verb correctly");
};

$vt->{'v7'}->("knees", "up", "mother", "brown");

my $newvt = {};
test::TISBLModule->install($newvt);

is_deeply($vt, $newvt, "VT installed correctly.");


1;