package test::Token;
use strict;
use Token;
use Ctx;
use Data::Dumper;
use Test::More tests=>170;

# Test token magic.
# Generate every possible combination of input/output stacks for a verb

for my $in ('', qw(: ; , .)) {
	for my $out ('', qw(: ; , .)) {
		my $verb = "\\${in}foo${out}";
		my $tok = Token->new($verb);
		
		ok($tok->input eq $in, "$verb: input stack correct");
		ok($tok->output eq $out, "$verb: output stack correct");
		ok($tok->content eq 'foo', "$verb: content correct");
		ok($tok->isa('Token::Verb'), "$verb: class correct");
	}
}

# And generate all possible kinds of noun.
for my $out ('', qw(: ; , .)) {
	for my $type (qw(# ')) {
		my $noun = "${out}${type}123";
		my $tok = Token->new($noun);
		ok($tok->isa('Token::Noun'), 'Token supertype correct');
		
		# this should die for nouns
		eval {
			$tok->input;
		};
		
		ok($@ =~ /^Somehow/,
			"There should be no input token on a noun");
		
		my $class;
		if ($type eq '#') {
			$class = 'Number';
		} else {
			$class = 'Word';
		}
		ok($tok->isa("Token::$class"), "Token type correct");
		
		ok($tok->content eq "123", "Token content correct");
	}
}

# Let's generate a Ctx and run some tokens onto it
# don't forget to run tests/Ctx.pm first
my $ins = Stack->new();
my $outs = Stack->new();
my $upexec = Stack->new();

my $ctx = new Ctx(
	input => $ins,
	output => $outs,
	upexec => $upexec
);

# Floats first
for my $out ('', qw(: ; , .)) {
	my $stack = $ctx->resolveStackName($out);
	my $tok = Token->new("${out}#3.5");
	$tok->run($ctx);	
	
	my $item = $stack->pop;
	
	ok($item->isa("SI::Float"), "Token ${out}#3.5 results in right class on right stack");
	is($item->val, 3.5, "   and the right value, too");
}

# then ints
for my $out ('', qw(: ; , .)) {
	my $stack = $ctx->resolveStackName($out);
	my $tok = Token->new("${out}#42");
	$tok->run($ctx);	
	
	my $item = $stack->pop;
	
	ok($item->isa("SI::Integer"), "Token ${out}#42 results in right class on right stack");
	is($item->val, 42, "   and the right value, too");
}

# then words
for my $out ('', qw(: ; , .)) {
	my $stack = $ctx->resolveStackName($out);
	my $tok = Token->new("${out}'fish");
	$tok->run($ctx);	
	
	my $item = $stack->pop;
	
	ok($item->isa("SI::String"), "Token ${out}'fish results in right class on right stack");
	is($item->val, "fish", "   and the right value, too");
}



1;