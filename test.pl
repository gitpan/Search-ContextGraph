# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

my $ENERGY = 10000;
# change 'tests => 1' to 'tests => last_test_to_print';
use Carp;
use Search::ContextGraph;

my $file = shift or die "Please provide a filename";

my $c = Search::ContextGraph->new();

$c->read_tdm( $file );
#$c->energize ( "t31", $ENERGY );
while (<>) {
	chomp;
	$c->energize( $_, $ENERGY) or print "Node not found\n";
	my $energy = $c->collect();
	print join "\n", map { "$_ \t ".$energy->{$_} }
		 sort { $energy->{$b} <=> $energy->{$a} } keys %{$energy};
	print "\n";
	$c->clear();
}

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

