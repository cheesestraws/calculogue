package InterpState;
use strict;
use Ctx;

# This is being done as an inside-out object for my own sanity.
# An inside-out object is a blessed scalar which contains an id
# which is used to index lexically-scoped variables inside my 
# module.  (Well, it can be indexed on any property of the
# reference - here I'm doing it by id)
#
# This gives bulletproof encapsulation, and, more importantly,
# stops me dealing with the @$@%{$... type syndrome that
# results from the 'blessed hash' school of object design
# (c.f. Token.pm)
#

my $id = -1;
my @ctxes = ();
my @verbtables = ();
my @trace = ();

sub new {
	my $class = shift;
	
	# increment id, which will be our new object id
	$id++;
	
	# set up a context stack for this object
	# a context stack is a list reference
	$ctxes[$id] = [];
	$verbtables[$id] = {};
	$trace[$id] = 0;
	
	my $self = $id;
	return bless(\$self, $class);
}

# start a new context
sub start {
	my ($self, $input, $output) = @_;
	
	# this is a reference, so updates in-place - 
	# the objects entry in @ctxes will still be updated.
	my $ctxes = $ctxes[$$self];
	
	# Find the parent's execution stack
	my $upexec = undef;
	# Do we even have a parent?
	if (@$ctxes) {
		# Yes!  So look at the last one
		my $oldctx = $ctxes->[-1];
		$upexec = $oldctx->exec;
	}
	
	# create new context
	my $ctx = Ctx->new(
		input => $input,
		output => $output,
		upexec => $upexec,
		owner => $self
	);
	
	push @$ctxes, $ctx;
}

sub stop {
	my $self = shift;
	my $ctxes = $ctxes[$$self];
	die "Stopping a nonexistent context. Probably an interpreter bug."
		unless @$ctxes;
	pop @$ctxes;
}

sub current {
	my $self = shift;
	my $ctxes = $ctxes[$$self];
	return $ctxes->[-1]
}

sub count {
	my $self = shift;
	my $ctxes = $ctxes[$$self];
	return scalar @$ctxes;
}

sub verbs {
	my $self = shift;
	return $verbtables[$$self];
}

sub runVerb {
	my ($self, $ctx, $verb, $input, $output) = @_;

	# check it exists
	die "No such verb: " . $verb
		unless $self->verbs->{$verb};
		
	# call the verb
	$self->verbs->{$verb}->($self, $ctx, $input, $output);
}

sub installVerb {
	my ($self, $name, $sub) = @_;
	$self->verbs->{$name} = $sub;
}

# destructor
sub DESTROY {
	my ($self) = @_;
	$trace[$$self] = undef;
	$ctxes[$$self] = undef;
	$verbtables[$$self] = undef;
}

sub traceOn {
	my ($self) = @_;
	$trace[$$self] = 1;
}

sub traceOff {
	my ($self) = @_;
	$trace[$$self] = 0;
}

sub trace {
	my ($self) = @_;
	return $trace[$$self];
}

sub traceWrite {
	my ($self, $str) = @_;
	print "[trace] $str\n" if $trace[$$self];
}


1;