package Stack;
use strict;
use SI;

# A stack is really a list.

sub new {
	my $class = shift;
	my $obj = [];
	return bless($obj, $class);
}

# The count method returns how many items are in the stack
sub count {
	my $self = shift;
	return scalar @$self;
}

# Push pushes an SI onto the stack.  It dies if the item is not an SI.
sub push {
	my ($self, $val) = @_;
	die "Pushing a non-SI.  This is probably an interpreter bug"
		unless $val->isa('SI');
	push @$self, $val;
}

sub sneak {
	my ($self, $val) = @_;
	die "Sneaking a non-SI.  This is probably an interpreter bug"
		unless $val->isa('SI');
	unshift @$self, $val;
}


sub pop {
	my $self = shift;
	die "Stack underflow." unless $self->count > 0;
	return pop @$self;
}

sub peek {
	my $self = shift;
	die "Stack underflow." unless $self->count > 0;
	return $self->[-1]; # last item of array
}

sub cloneTo {
	my ($self, $other) = @_;
	die "Clone destination has no push method.  This is probably an interpreter bug."
		unless $other->can('push');
	
	# copy all elements to destination;
	for my $i (@$self) {
		$other->push($i);
	}
}

sub trace {
	my $self = shift;
	my @displays = map { $_->trace } @$self;
	return join " ", reverse @displays;
}


# 8X --- --- --- --- --- ---
package Stack::Dummy;
use strict;
use base 'Stack';

# This is a dummy stack to use in the root context
sub push {
	die "You cannot push onto input/output or parent exec from the root context.";
}

sub pop {
	die "You cannot pop from input/output or parent exec from the root context.";
}

sub peek {
	die "You cannot peek at input/output or parent exec from the root context.";
}

sub cloneTo {
	die "Cannot clone a dummy stack for the root context.  This is probably an interpreter bug.";
}

1;