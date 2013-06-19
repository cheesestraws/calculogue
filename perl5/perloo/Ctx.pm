package Ctx;
use strict;
use Stack;
use Token;

# All of this is immutable.
# (words to live by...)

sub new {
	my $class = shift;
	my %opts = @_;
	die "You must provide an input stack to Ctx->new. Probably an interpreter bug."
		unless exists $opts{'input'};
	die "You must provide an output stack to Ctx->new. Probably an interpreter bug."
		unless exists $opts{'output'};
	die "You must provide an upexec stack to Ctx->new. Probably an interpreter bug."
		unless exists $opts{'upexec'};
		
	# create the object
	my $self = {};
	$self->{'pri'} = Stack->new;
	$self->{'sec'} = Stack->new;
	$self->{'exec'} = Stack->new;
	
	# Wire it up to the stacks it was passed in.
	__install_stack_or_dummy($self, 'input', $opts{'input'});
	__install_stack_or_dummy($self, 'output', $opts{'output'});
	__install_stack_or_dummy($self, 'upexec', $opts{'upexec'});
	
	$self->{'owner'} = $opts{'owner'} || undef;
	
	bless $self, $class;
}


# this function is a convenience for the constructor
# it isn't private.  There is no private in perl.
# It installs the $stack value in $hashref->{$name} iff
# it is actually a stack.
sub __install_stack {
	my ($hashref, $name, $stack) = @_;
	die ucfirst($name) . " is not a stack.  Probably an interpreter bug." unless
		$stack->can('push') && $stack->can('pop') && $stack->can('peek');
		
	$hashref->{$name} = $stack;
}

# this installs a stack or a dummy stack if the stack is undef
# Stack::Dummy intercepts stack ops to make error messages nicer.
sub __install_stack_or_dummy {
	my ($hashref, $name, $stack) = @_;
	if ($stack) {
		__install_stack($hashref, $name, $stack);
	} else {
		$hashref->{$name} = Stack::Dummy->new;
	}
}

# some accessors
# $_[0] is the first parameter, i.e. $self
sub pri { return $_[0]->{'pri'}; }
sub sec { return $_[0]->{'sec'}; }
sub exec { return $_[0]->{'exec'}; }

sub input { return $_[0]->{'input'}; }
sub output { return $_[0]->{'output'}; }
sub upexec { return $_[0]->{'upexec'}; }

# the owner is an InterpState object.
sub owner { return $_[0]->{'owner'}; }


# Turn a stack char -> a Stack
sub resolveStackName {
	my ($self, $name, $opts) = @_;
	
	return ({
		''  => $self->pri,
		':' => $self->sec,
		',' => $self->exec,
		';' => $self->upexec,
		'.' => ($opts->{'input'} ? $self->input : $self->output)
	})->{$name};
}


# run a Ctx
sub go {
	my ($self) = @_;
	
	my ($item, $tok);
	while (@{$self->exec}) {
		# grab a token
		$item = $self->exec->pop;
		
		# Check it's a string on the stack.  Bail out otherwise.
		die "Bad item (" . $item . ") on exec stack."
			unless $item->isa("SI::String");
			
		$self->owner->traceWrite("After token: " . $item->val);
			
		# Then parse it
		$tok = Token->new($item->val);
		
		# and run it
		$tok->run($self);
		
		$self->trace;
		$self->owner->traceWrite("");
	}
}


sub runVerb {
	my ($self, $verb, $instack, $outstack) = @_;
	# promptly make this our owner's problem
	# as that maintains the verbtable
	$self->owner->runVerb($self, $verb, $instack, $outstack);
}

sub trace {
	my ($self) = @_;
	if ($self->owner->trace) {
		if ($self->owner->count == 1) {
			$self->owner->traceWrite("In topmost context.");
		} else {
			my $ctxid = $self->owner->count - 1;
			$self->owner->traceWrite("$ctxid stacks down from root.");
		}
		for my $i ([" PRI", $self->pri],
		           [" SEC", $self->sec],
		           ["EXEC", $self->exec]) {
			my $str = $i->[0] . ": TOP => ";
			$str .= $i->[1]->trace;
			$self->owner->traceWrite($str);
		}
	}
}

1;