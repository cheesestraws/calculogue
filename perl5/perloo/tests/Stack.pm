package tests::Stack;
use strict;
use Stack;
use SI;
use Test::More tests=>9;

my $stack = Stack->new;
ok($stack->isa('Stack'), 'Stack is of the right class');
ok($stack->count == 0, '    and is empty.');

$stack->push(SI::Str("hello"));
ok($stack->count == 1, '    pushing seems to work.');

eval {
	$stack->push("This is a string, not an SI");
};
ok($@ =~ /^Pushing a non-SI/, '   except when it\'s not an SI');

$stack->push(SI::Int(4));
my $var = $stack->pop;
ok($var->val == 4, 'Pushing then popping works');

eval {
	# pops 'hello' and then tries to pop a nonexistent item
	$stack->pop;
	$stack->pop;
};

ok($@ =~ /^Stack underflow/, '    except when the stack is empty');


$stack->push(SI::Str("hello"));
$var = $stack->peek;
ok($var->val eq 'hello', '    peeking works too');

eval {
	# pops 'hello' and then tries to pop a nonexistent item
	$stack->pop;
	$stack->peek;
};
ok($@ =~ /^Stack underflow/, '    except when the stack is empty');

# check clone behaviour
$stack->push(SI::Str('hello'));
$stack->push(SI::Str('world'));
$stack->push(SI::Int(42));

my $stack2 = Stack->new;
$stack->cloneTo($stack2);

is_deeply($stack, $stack2, 'Clone semantics are correct.');



1;