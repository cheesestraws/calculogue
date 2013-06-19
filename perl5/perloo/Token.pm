package Token;
use strict;
use SI;

# This deserves some explanation.
# Each token knows how to run itself based on a Ctx.
# The alleged constructor of Token never actually returns a Token,
# it instead returns a subclass of Token specialised to the kind
# of token it is.
#
# I believe Cocoa does something reminiscent of this with NSString.
# 
# Yes, this is bad for inheritance, and if it were likely that I'd be
# adding new token types willy-nilly, then I'd be more inclined to
# make this nicely modular.  But all the token types there will ever be
# are in this file (since this is a slightly artificial problem).
#
# It could be modularised by each subclass pushing up to the Token
# class a regex to parse it into input/output/token; and then testing
# those regexes in order.

sub new {
	my ($class, $tok) = @_;
	my $self = {};
	
	# parse the token string with regexes.  This should be tolerably
	# clear...
	if ($tok =~ /^\\([.,:;]?)(.*?)([.,:;]?)$/) {
		$self = {
			input => $1,
			content => $2,
			output => $3,
			original => $tok
		};
		return bless($self, $class . '::Verb');
	} elsif ($tok =~ /^([.,:;]?)([#'])(.*)$/) {
		$self = {
			output => $1,
			content => $3,
			original => $tok
		};
		
		# check what type to create
		if ($2 eq '#') {
			return bless($self, $class . "::Number");
		} elsif ($2 eq "'") {
			return bless($self, $class . "::Word");
		} else {
			die "Bad nountype.  Probably an interpreter bug - did you update the regex but not the if in Token.pm?";
		}
	} else {
		die "Bad token: " . $tok;
	}
}

sub run {
	die "Somehow ->run() got called on an abstract Token.  Probably an interpreter error.";
}

sub input { return $_[0]->{'input'}; }
sub output { return $_[0]->{'output'}; }
sub content { return $_[0]->{'content'}; }

# this returns the original string
sub original { return $_[0]->{'original'}; }



# 8X----- ----- ----- ----- ----- -----
package Token::Verb;
use strict;
use base 'Token';

sub run {
	my ($self, $ctx) = @_;
	
	my $input = $ctx->resolveStackName($self->input, { input => 1 });
	my $output = $ctx->resolveStackName($self->output);
	
	$ctx->runVerb($self->content, $input, $output);
}


# 8X----- ----- ----- ----- ----- -----
package Token::Noun;
use strict;
use base 'Token';

# a base class for nouns
sub input { die "Somehow ->input() got run on a noun token.  Probably an interpreter bug."; }



# 8X----- ----- ----- ----- ----- -----
package Token::Number;
use strict;
use base 'Token::Noun';

sub run {
	my ($self, $ctx) = @_;
	my $stack = $ctx->resolveStackName($self->output);
	
	if ($self->content =~ /\./) {
		# We have a decimal point
		$stack->push(SI::Flt($self->content));
	} else {
		$stack->push(SI::Int($self->content));
	}
}


# 8X----- ----- ----- ----- ----- -----
package Token::Word;
use strict;
use base 'Token::Noun';

sub run {
	my ($self, $ctx) = @_;
	my $stack = $ctx->resolveStackName($self->output);
	
	$stack->push(SI::Str($self->content));
}


1;