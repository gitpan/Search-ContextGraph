#!/usr/bin/perl -w

use Search::ContextGraph;
use Test::More tests => 6;

my %docs = (
  'First Document' => { 'elephant' => 2, 'snake' => 1 },
  'Second Document' => { 'camel' => 1, 'pony' => 1 },
  'Third Document' => { 'snake' => 2, 'constrictor' => 1 },
);

my $cg = Search::ContextGraph->new();
ok($cg, "have Search::ContextGraph object");
$cg->add_documents( %docs );

my $results = $cg->search('snake');

is(scalar(keys(%$results)), 2, "only two matches");
ok($results->{"First Document"}, "contains first document");
ok($results->{"Third Document"}, "contains third document");

is(sprintf("%2.2f", $results->{"First Document"}), 10.94, "relevance for first right");
is(sprintf("%2.2f", $results->{"Third Document"}), 18.53, "relevance for third doc right");
 
