package SI;
use strict;

# "A class is just a package"
# This package and its subpackages represent stack items.

# In perl, an object is any blessed reference.  Stack items,
# therefore, are blessed scalars.

# The val method returns the value.  SIs are as near immutable
# as they really can be.
sub val {
	my $self = shift;
	return $$self;
}

# some shortcut helper methods
sub Str {
	my $value = shift;
	return SI::String->new($value);
}

sub Flt {
	my $value = shift;
	return SI::Float->new($value);
}

sub Int {
	my $value = shift;
	return SI::Integer->new($value);
}

sub trace {
	# Magic be here.
	# This takes a stack item and returns it as a string suitable for
	# traces.  To do this I need to turn its type into a short three-letter
	# string too, so I can show things like
	#	int.4 flt.7
	# To do this: look at the class of the object, chop off the bit before the
	# ::, remove all lowercase vowels from it, force it to lowercase, and take  
	# the first three chars.
	# I warned you!
	
	my ($self) = @_;
	my $class = ref($self);
	$class =~ s/^.*:://;
	$class =~ tr/aeiou//d;
	$class = lc(substr($class, 0, 3));
	
	return $class . "." . $self->val;
}


# 8X --- --- --- --- --- --- --- --- ---
package SI::String;
use strict;
use base 'SI';

# A constructor
sub new {
	my ($class, $value) = @_;
	my $var = $value;
	my $ref = \$var;
	
	return bless($ref, $class);
}


# 8X --- --- --- --- --- --- --- --- ---

package SI::Number;
use strict;
use base 'SI';
use Scalar::Util qw(looks_like_number);

# A constructor
sub new {
	my ($class, $value) = @_;
	my $var = $value;
	die "Not a number" unless looks_like_number($value);
	my $ref = \$var;
	
	return bless($ref, $class);

}

# 8X --- --- --- --- --- --- --- --- ---

package SI::Float;
use strict;
use base 'SI::Number';

# 8X --- --- --- --- --- --- --- --- ---

package SI::Integer;
use strict;
use base 'SI::Number';



# 'use'd files have to return a true result
1;
