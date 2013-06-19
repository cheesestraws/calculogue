package tests::SI;
use Test::Simple tests=>11;
use SI;

# SI::String tests
my $str = SI::String->new("hello");

ok($str->isa('SI::String'), 'str is in the right class');
ok($str->isa('SI'), '    and inheritance works');
ok($str->val eq "hello", '    and has the right value.');



# SI::Number tests
my $int = SI::Integer->new(43);
ok($int->isa('SI::Integer'), 'int is in the right class');
ok($int->isa('SI'), '    and inheritance works');
ok($int->val == 43, '    and has the right value');

my $float = SI::Float->new(2.5);
ok($float->val == 2.5, '    and so does the float');

# Test for errors
eval {
	my $error = SI::Number->new("hello");
};
ok($@ =~ /^Not a number/, 'Number objects must only contain numbers.');


# Shortcut functions
my $str1 = SI::Str("foo");
ok($str1->isa('SI::String'), 'SI::Str result is in the right class');
my $int1 = SI::Int(4);
ok($int1->isa('SI::Integer'), 'SI::Int result is in the right class');
my $flt1 = SI::Flt(4.4);
ok($flt1->isa('SI::Float'), 'SI::Flt result is in the right class');


1;