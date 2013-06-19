package tests::InterpState;
use strict;

use InterpState;
use Test::More tests => 4;


my $state = InterpState->new();
is($state->count, 0, 'InterpState is correctly empty.');

$state->start(undef, undef);
is($state->count, 1, 'Starting a context works.');
ok($state->current->isa('Ctx'), 'Context is a Ctx.');

# check that exec is propagating correctly
my $oldexec = $state->current->exec;
$state->start(undef, undef);
is($state->current->upexec, $oldexec, "New context has access to its parent's exec.");

1;