package Stdlib;
use strict;
use TISBLModule;
use Stack;
use base 'TISBLModule';


verb { "exec" } is {
	die "\\exec expected an integer count but didn't get one"
		unless input->peek->isa('SI::Integer');

	state->start(input, output);
	my $ctx = state->current;
	my $count = input->pop->val;
	
	for my $i (1..$count) {
		my $tok = input->pop;
		$ctx->exec->push($tok);
	}
	
	$ctx->go;
	state->stop;
};



verb { "verb" } is {
	my $name = input->pop->val;
	
	die "\\verb expected an integer count but didn't get one"
		unless input->peek->isa('SI::Integer');
		
	my $count = input->pop->val;
	
	# the template stack
	my $stack = Stack->new;
	
	# copy tokens onto it
	for my $i (1..$count) {
		my $tok = input->pop;
		$stack->push($tok);
	}

	# now magic.  We have a lexical $stack...
	# create a new native verb that wraps it
	my $proc = verbsub {
		state->start(input,output);
		my $ctx = state->current;
		$stack->cloneTo($ctx->exec);
		$ctx->go;
		state->stop;
	};
	state->installVerb($name, $proc);
};



verb { "if" } is {
	die "\\if expected an integer count but didn't get one"
		unless input->peek->isa('SI::Integer');
		
	my $count = input->pop->val;
	state->start(input, output);
	my $ctx = state->current;
	
	for my $i (1..$count) {
		my $tok = input->pop;
		$ctx->exec->push($tok);
	}

	my $condition = input->pop;
	unless ($condition->isa('SI::Number') && ($condition->val == 0)) {
		$ctx->go;
	}
	
	state->stop;
};



verb { "while" } is {
	die "\\while expected an integer count but didn't get one"
		unless input->peek->isa('SI::Integer');
		
	my $count = input->pop->val;
	
	# the template stack
	my $stack = Stack->new;
	
	# copy tokens onto it
	for my $i (1..$count) {
		my $tok = input->pop;
		$stack->push($tok);
	}
	
	my $endloop = 0;
	do {
		my $condition = input->pop;
		if ($condition->isa('SI::Number') && ($condition->val == 0)) {
			$endloop = 1;
		} else {
			state->start(input, output);
			my $ctx = state->current;
			$stack->cloneTo($ctx->exec);
			$ctx->go;
			state->stop;
		}
	} until ($endloop == 1);
};


verb { "not" } is {
	my $condition = input->pop;
	if ($condition->isa('SI::Number') && ($condition->val == 0)) {
		output->push(SI::Int(1));
	} else {
		output->push(SI::Int(0));
	}
};


verb { "swap" } is {
	my $a = input->pop;
	my $b = input->pop;
	output->push($a);
	output->push($b);
};

verb { "dup" } is {
	my $item = input->peek;
	output->push($item);
};


verb { "rm" } is {
	input->pop;
};


verb { "mv" } is {
	my $item = input->pop;
	output->push($item);
};


verb { "multipop" } is {
	die "\\multipop expected an integer count but didn't get one"
		unless input->peek->isa('SI::Integer');
		
	my $count = input->pop->val;
	for my $i (1..$count) {
		my $item = input->pop;
		output->push($item);
	}
};


verb { "+" } is {
	my $sec = input->pop;
	my $fst = input->pop;
	
	if ($fst->isa("SI::String") || $sec->isa("SI::String")) {
		output->push(SI::Str($fst->val . $sec->val));
	} elsif ($fst->isa("SI::Float") || $sec->isa("SI::Float")) {
		output->push(SI::Flt($fst->val + $sec->val));
	} else {
		output->push(SI::Int($fst->val + $sec->val));
	}
};

sub sub_si {
	my ($fst, $sec) = @_;
	return substr($fst, 0, 0-$sec);
}

sub sub_ss {
	my ($fst, $sec) = @_;
	$fst =~ s/[$sec]//g;
	return $fst;
}

verb { "-" } is {
	my $sec = input->pop;
	my $fst = input->pop;
	
	if ($fst->isa("SI::Integer") && $sec->isa("SI::Integer")) {
		output->push(SI::Int($fst->val - $sec->val));
	} elsif ($fst->isa("SI::Number") && $sec->isa("SI::String")) {
		output->push(SI::Str(sub_si($sec->val, $fst->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::Number")) {
		output->push(SI::Str(sub_si($fst->val, $sec->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::String")) {
		output->push(SI::Str(sub_ss($fst->val, $sec->val)));
	} else {
		output->push(SI::Flt($fst->val - $sec->val));
	}
};


sub mul_sf {
	my ($str, $iter) = @_;
	my $buf = $str x int($iter); # yes, perl does string multiplication
	# but only on integers
	$iter = $iter - int($iter);
	my $partial = substr($str, 0, length($str) * $iter);
	return $buf . $partial;
}

sub mul_ss {
	my ($str, $expand) = @_;
	my $initchar = substr($expand,0,1); # first char
	$str =~ s/$initchar/$expand/g;
	return $str;
}

verb { "*" } is {
	my $sec = input->pop;
	my $fst = input->pop;
	
	if ($fst->isa("SI::Integer") && $sec->isa("SI::Integer")) {
		output->push(SI::Int($fst->val * $sec->val));
	} elsif ($fst->isa("SI::Number") && $sec->isa("SI::String")) {
		output->push(SI::Str(mul_sf($sec->val, $fst->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::Number")) {
		output->push(SI::Str(mul_sf($fst->val, $sec->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::String")) {
		output->push(SI::Str(mul_ss($fst->val, $sec->val)));
	} else {
		output->push(SI::Flt($fst->val * $sec->val));
	}
};


sub div_sf {
	my ($str, $int) = @_;
	my $len = length($str) / $int;
	return substr($str, 0, $len);
}
sub div_ss {
	my ($haystack, $needle) = @_;
	my $initchar = substr($needle,0,1); # first char
	$haystack =~ s/$needle/$initchar/g;
	return $haystack;
}

verb { "div" } is {
	my $sec = input->pop;
	my $fst = input->pop;
	
	if ($fst->isa("SI::Number") && $sec->isa("SI::Number")) {
		output->push(SI::Flt($fst->val / $sec->val));
	} elsif ($fst->isa("SI::Number") && $sec->isa("SI::String")) {
		output->push(SI::Str(div_sf($sec->val, $fst->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::Number")) {
		output->push(SI::Str(div_sf($fst->val, $sec->val)));
	} elsif ($fst->isa("SI::String") && $sec->isa("SI::String")) {
		output->push(SI::Str(div_ss($fst->val, $sec->val)));
	}
};


verb { "n" } is {
	output->push(SI::Str(input->pop->val . "\n"));
};


verb { "_" } is {
	output->push(SI::Str(input->pop->val . " "));
};


verb { "string?" } is {
	my $item = input->pop;
	output->push(SI::Int(1)) if $item->isa('SI::String');
	output->push(SI::Int(0)) unless $item->isa('SI::String');
};


verb { "number?" } is {
	my $item = input->pop;
	output->push(SI::Int(1)) if $item->isa('SI::Number');
	output->push(SI::Int(0)) unless $item->isa('SI::Number');
};


verb { "integer?" } is {
	my $item = input->pop;
	output->push(SI::Int(1)) if $item->isa('SI::Integer');
	output->push(SI::Int(0)) unless $item->isa('SI::Integer');
};


verb { "float?" } is {
	my $item = input->pop;
	output->push(SI::Int(1)) if $item->isa('SI::Float');
	output->push(SI::Int(0)) unless $item->isa('SI::Float');
};


verb { "eq?" } is {
	my $a = input->pop;
	my $b = input->pop;
	
	if ($a->isa("SI::String") && $b->isa("SI::String") &&
		($a->val eq $b->val)) {
			output->push(SI::Int(1));
	} elsif ($a->isa("SI::Number") && $b->isa("SI::Number") &&
		($a->val == $b->val)) {
			output->push(SI::Int(1));
	} else {
		output->push(SI::Int(0));
	}
};


verb { "die" } is { die; };


verb { "out" } is {
	my $a = input->pop;
	print $a->val;
};


verb { "in" } is {
	my $line = <STDIN>;
	chomp($line);  # kill cr/lf
	output->push(SI::Str($line));
};


verb { "trace=0" } is {
	state->traceOff;
};


verb { "trace=1" } is {
	state->traceOn;
};

1;