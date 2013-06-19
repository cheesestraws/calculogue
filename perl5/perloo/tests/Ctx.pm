package tests::Ctx;
use strict;
use Test::More tests=>18;

use Ctx;

# Check constraints on constructors.
my $ctx;

eval {
	$ctx = Ctx->new();
};
ok($@ =~ /^You must provide an input stack/, "Ctx constructor has to have an input stack.");

eval {
	$ctx = Ctx->new(
		input => undef
	);
};
ok($@ =~ /^You must provide an output stack/, "Ctx constructor has to have an output stack.");


eval {
	$ctx = Ctx->new(
		input => undef,
		output => undef
	);
};
ok($@ =~ /^You must provide an upexec stack/, "Ctx constructor has to have an output stack.");


# check that undef magically turns into Stack::Dummy

$ctx = Ctx->new(
	input => undef,
	output => undef,
	upexec => undef
);

ok($ctx->input->isa('Stack::Dummy'), 'Dummification magic on input stack works.');
ok($ctx->output->isa('Stack::Dummy'), 'Dummification magic on output stack works.');
ok($ctx->upexec->isa('Stack::Dummy'), 'Dummification magic on upexec stack works.');

# and that passed-in stacks make it through
my @stacks = map { Stack->new; } (0,1,2);
$ctx = Ctx->new(
	input => $stacks[0],
	output => $stacks[1],
	upexec => $stacks[2]
);

ok($ctx->input == $stacks[0], 'Input stack correctly installed');
ok($ctx->output == $stacks[1], 'Output stack correctly installed');
ok($ctx->upexec == $stacks[2], 'Upexec stack correctly installed');

ok($ctx->pri->isa('Stack'), 'Primary stack is of the right type.');
ok($ctx->sec->isa('Stack'), 'Secondary stack is of the right type.');
ok($ctx->exec->isa('Stack'), 'Exec stack is of the right type.');

is($ctx->resolveStackName(""), $ctx->pri, 'Primary stack can be looked up by name.');
is($ctx->resolveStackName(":"), $ctx->sec, 'Secondary stack can be looked up by name.');
is($ctx->resolveStackName(","), $ctx->exec, 'Execution stack can be looked up by name.');
is($ctx->resolveStackName(";"), $ctx->upexec, 'Parent execution stack can be looked up by name.');
is($ctx->resolveStackName(".", {input => 1}), $ctx->input, 'Input stack can be looked up by name.');
is($ctx->resolveStackName("."), $ctx->output, 'Output stack can be looked up by name.');



1;