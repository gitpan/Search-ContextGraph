# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################
# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 14;

BEGIN {
	use_ok( 'Search::ContextGraph' );
}

my $g = eval { Search::ContextGraph->new() };
is($@, '', 'created new graph object');
is(ref($g), 'Search::ContextGraph', 'object creation');

eval { $g->load( "sample.tdm" ) };
is($@, '', 'loaded sample tdm file');

my $results = eval { $g->search( "t11" ) };
is($@, '', 'canned search ran correctly');
is( scalar keys %{$results}, 156, 'canned search gives correct results' );

my $start_e = eval { $g->get_initial_energy() };
is($@, '', 'able to ask for initial energy');
is( $start_e, 10000, 'initial energy is at default value' );

my $new_e = eval { $g->set_initial_energy(12) };
is($@, '', 'able to reset initial energy');
is( $g->get_initial_energy(), 12, 'initial energy set to new value' );

my $thresh = eval { $g->get_threshold() };
is($@, '', 'able to ask for threshold');
is( $thresh, 1, 'threshold is at default value' );

my $new_thresh = eval { $g->set_threshold(12) };
is($@, '', 'able to reset threshold');
is( $g->get_threshold(), 12, 'threshold set to new value' );