package Search::ContextGraph;

use strict;
use warnings;
use Carp;
use base "Storable";

our $VERSION = '0.07';

# If you are in an environment that can't compile XS
# modules, comment out the next three lines of code

use DynaLoader;
our @ISA = qw/DynaLoader Storable/;
bootstrap Search::ContextGraph $VERSION;





our @log_table = qw/0 1.00 1.69 2.10 2.39 2.61 2.79 2.95 3.08
				    3.20 3.30 3.40 3.48 3.56 3.64 3.71 3.77 
				    3.83 3.89 3.94 4.00 4.04 4.09 4.14 4.18 
				    4.22 4.26 4.30 4.33 4.37 4.40 4.43 4.47 
				    4.50 4.53 4.56 4.58 4.61 4.64 4.66 4.69 
				    4.71 4.74 4.76 4.78 4.81 4.83 4.85 4.87 
				    4.89 4.91/;
our %log_lookup;
my $count = 0;
$log_lookup{$count++} = $_ foreach @log_table;




=head1 NAME

Search::ContextGraph - Run searches using a contextual network graph

=head1 SYNOPSIS

  use Search::ContextGraph;

  my %docs = (
    'First Document'  => { 'elephant' => 2, 'snake' => 1 },
    'Second Document' => { 'camel' => 1, 'pony' => 1 },
    'Third Document'  => { 'snake' => 2, 'constrictor' => 1 },
  );

  my $cg = Search::ContextGraph->new();
  $cg->add_documents( %docs );

  # Regular word search
  my ( $docs, $words ) = $cg->search('snake');

  # Document similarity search
  my ( $docs, $words ) = $cg->find_similar('First Document');

  # Search on a little bit of both...
  my ( $docs, $words ) = 
    $cg->mixed_search( { docs  => [ 'First Document' ],
                         terms => [ 'snake', 'pony' ]
                     );

  # Print out result set of returned documents
  foreach my $k ( sort { $docs->{$b} <=> $docs->{$a} }
      keys %{ $docs } ) {
      print "Document $k had relevance ", $docs->{$k}, "\n";
  }

  # Store the graph for future generations
  $cg->store( "filename" );

  # Reload it
  my $new = Search::ContextGraph->retrieve( "filename" );



=head1 DESCRIPTION

Search a document collection using a spreading activation search.   The search
algorithm represents the collection as a set of term and document nodes,
connected to one another based on a co-occurrence matrix.  If a word occurs in
a document, we create an edge between the appropriate term and document node.
Searches take place by spreading energy from a query node along the edges of 
the graph according to some simple rules.  All result nodes exceeding a threshold 
T are returned.   You can read a full description of this algorithm
at L<http://www.nitle.org/papers/Contextual_Network_Graphs.pdf>.

The search engine gives expanded recall (relevant results even when there is no
keyword match) without incurring the kind of computational and patent issues
posed by latent semantic indexing (LSI).

=head1 METHODS

=over

=item new %PARAMS

Object constructor.   Possible parameters:

=over

=item * debug LEVEL

Set this to 1 or 2 to turn on verbose debugging output

=item * xs

When true, tells the module to use compiled C internals.  This reduces
memory requirements by about 60%, but actually runs a little slower than the 
pure Perl version.  Don't bother to turn it on unless you have a huge graph. 
Default is pure Perl.

BUG: using the compiled version makes it impossible to store the graph to disk.

=item * START_ENERGY

Initial energy to assign to a query node.  Default is 100.

=item * ACTIVATE_THRESHOLD

Minimal energy needed to propagate search along the graph.  Default is 1.

=item * COLLECT_THRESHOLD

Minimal energy needed for a node to enter the result set.  Default is 1.

=back 

=cut


sub new {
	my ( $class, %params) = @_;
	my $obj = bless 
		{ debug => 0,
		  fast => 0,
		  START_ENERGY => 100,
		  ACTIVATE_THRESHOLD => 1,
		  COLLECT_THRESHOLD => .2,
	      %params,
	      neighbors => {}, 

		}, 
	$class;
	if ( $obj->{'xs'} ) {
		my $graph = Search::ContextGraph::Graph->new(
			$obj->{START_ENERGY},
			$obj->{ACTIVATE_THRESHOLD},
			$obj->{COLLECT_THRESHOLD},
		);
		$obj->{Graph} = $graph;
		$obj->{'next_free_id'} = 0;
		$obj->{'node_map'} = {};
		
	}
	return $obj;
		
}

=item [get|set]_activate_threshold

Accessor for node activation threshold value.  This value determines how far 
energy can spread in the graph.  Lower it to increase the number of results.
Default is 1.

=cut

sub get_activate_threshold {    $_[0]->{'ACTIVATE_THRESHOLD'} }
sub set_activate_threshold {
	my ( $self, $threshold ) =  @_;
	croak "Can't set activate threshold to zero"
		unless $threshold;
	croak "Can't set activate threshold to negative value"
		unless $threshold > 0;
	$self->{'ACTIVATE_THRESHOLD'} = $_[1]; 
}


=item [get|set]_collect_threshold

Accessor for collection threshold value.  This determines how much energy a
node must have to make it into the result set.  Lower it to increase the 
number of results.   Default is 1.

=cut

sub get_collect_threshold {  
	return ( $_[0]->{'xs'} ? 
		$_[0]->{Graph}->collectionThreshold :
		$_[0]->{'COLLECT_THRESHOLD'})
 }
 
sub set_collect_threshold {	
	 my ( $self, $newval ) = @_;
	 
	 $newval ||=0;
 
	 $self->{Graph}->collectionThreshold( $newval )
	 	if $self->{'xs'};
	 
	 $self->{'COLLECT_THRESHOLD'} = $newval || 0;
	 return 1;
}

=item set_debug_mode [012]

Turns debugging on or off.  1 is verbose, 2 is very verbose, 0 is off.

=cut

sub set_debug_mode {
	my ( $self, $mode ) = @_;
	$self->{'debug'} = $mode;
}


sub get_dist_weight { $_[0]->{'dist_weight'} }
sub set_dist_weight { $_[0]->{'dist_weight'} = $_[1] }



=item [get|set]_initial_energy

Accessor for initial energy value at the query node.  This controls how 
much energy gets poured into the graph at the start of the search.
Increase this value to get more results from your queries.

=cut

sub get_initial_energy { $_[0]->{'START_ENERGY'} }
sub set_initial_energy { 
	my ( $self, $start_energy ) = @_;
	croak "Can't set initial energy to zero"
		unless $start_energy;
	croak "Can't set initial energy to negative value"
		unless $start_energy > 0;
	$self->{'START_ENERGY'} = $start_energy ;
	}

=item load_from_tdm TDM_FILE [, LM_FILE]

Opens and loads a term-document matrix (TDM) file to initialize the graph.
The TDM encodes information about term-to-document links.
For notes on the proper file format, see the README file
Note that document-document links are NOT YET IMPLEMENTED.

=cut

sub load_from_tdm {
	my  ( $self, $file ) = @_;
	croak "TDM file $file does not exist" unless -f $file;
	return if $self->{'loaded'};
	$self->_read_tdm( $file );
	$self->{'loaded'} = 1;
}




=item raw_search @NODES

Given a list of nodes, returns a hash of nearest nodes with relevance values,
in the format NODE => RELEVANCE, for all nodes above the threshold value. 
(You probably want one of L<search>, L<find_similar>, or L<mixed_search> instead).

=cut

sub raw_search {
	my ( $self, @query ) = @_;

	
	my $results_ref;
	
	### XS VERSION  #######
	if ( $self->{'xs'} ) {
		
		$self->{'Graph'}->reset_graph();
		my $map = $self->{'node_map'};

		foreach my $node ( map { $map->{$_} } @query ) {

			$self->{'Graph'}->energize_node( $node, $self->{'START_ENERGY'}, 1);
		}
		my $results_arref = $self->{Graph}->collect_results();

		my %hash;

		foreach my $res ( @{$results_arref} ) {
			my $key =  $self->{'id_map'}[$res->[0]] || next;
			$hash{$key} = $res->[1] || 0;
		}
		$results_ref = \%hash;
		
				
	#### PURE PERL VERSION #####
	} else {
	    $self->_clear();
		foreach ( @query ) {
			$self->_energize( $_, $self->{'START_ENERGY'});
		}
		$results_ref = $self->_collect();
	}	
	 
	return $results_ref;
}

=item debug_on, debug_off

Toggles debug mode

=cut

sub debug_on { $_[0]->{'debug'} = 1 }
sub debug_off { $_[0]->{'debug'} = 0 }


# Opens and reads a term-document matrix (TDM) file.  The format for this file
# is described in the README


sub _read_tdm {
	my ( $self, $file ) = @_;
	print "Loading TDM...\n" if $self->{'debug'} > 1;

	croak "File does not exist" unless -f $file;
	open my $fh, $file or croak "Could not open $file: $!";
	for ( 1..4 ){
		my $skip = <$fh>;
	}
	my %neighbors;
	my $doc = 0;	
	
	
	######### XS VERSION ##############
	if ( $self->{'xs'} ) {
		
		my $map = $self->{'node_map'}; # shortcut alias
		while (<$fh>) {
			chomp;
			my $dindex = $self->_add_node( "D:$doc", 2 );
			#warn "Added node $doc\n";
			my ( $count, %vals ) = split;
			while ( my ( $term, $edge ) = each %vals ) {
				$self->{'term_count'}{$term}++;
				my $tnode = "T:$term";
				
				my $tindex = ( defined $map->{$tnode} ?
								$map->{$tnode} : 
							 	$self->_add_node( $tnode, 1 )
							);
				$self->{Graph}->set_edge( $dindex, $tindex, $edge );				
			}
			$doc++;
		}
		
	####### PURE PERL VERSION ##########
	} else {
		while (<$fh>) {
			chomp;
			my $dnode = "D:$doc";
			my ( $count, %vals ) = split;
			while ( my ( $term, $edge ) = each %vals ) {
				$self->{'term_count'}{$term}++;
				my $tnode = "T:$term";
				
				$neighbors{$dnode}{$tnode} = $edge;
				$neighbors{$tnode}{$dnode} = $edge;
			}
			$doc++;
		}
		$self->{'neighbors'} = \%neighbors;	
	}
	
	print "Loaded.\n" if $self->{'debug'} > 1;
	$self->{'from_TDM'} = 1;
	$self->{'doc_count'} = $doc;
}



# XS version only
#
# This sub maintains a mapping between node names and integer index
# values. 

sub _add_node {
	my ( $self, $node_name, $type ) = @_;
	croak "Must provide a type" unless $type;
	croak "Must provide a node name" unless $node_name;
	croak "This node already exists" if 
		 $self->{'node_map'}{$node_name};
		
	my $new_id = $self->{'next_free_id'}++;
	$self->{'node_map'}{$node_name} = $new_id;
	$self->{'id_map'}[$new_id] = $node_name;
	$self->{'Graph'}->add_node( $new_id, $type );
	
	return $new_id;
}

=item add_documents %DOCS

Load up the search engine with documents in the form
TITLE => WORDS, where WORDS is either a  reference to a hash of terms
and occurence counts, or a reference to an array of words.
For example:

	TITLE => { WORD1 => COUNT1, WORD2 => COUNT2 ... }

or

	TITLE => [ WORD1, WORD2, WORD3 ]


=cut

sub add_documents {

	my ( $self, %incoming_docs ) = @_;

	my @doc_list = keys %incoming_docs; # Make sure these remain in same order
	
	# Add word and document nodes to the graph
	foreach my $doc ( @doc_list ) {
		my $dnode = 'D:'.$doc;
		croak "Tried to add document with duplicate title: \"$doc\"\n"
			if exists( $self->{'neighbors'}{$dnode} ) or 
			exists $self->{'node_map'}{$dnode};
			
		croak "Not a reference" unless ref( $incoming_docs{$doc});
		my $type = ref( $incoming_docs{$doc});
		my @list;
		if ( $type eq 'HASH' ) {
			@list = keys %{ $incoming_docs{$doc} }
		} elsif ( $type eq 'ARRAY' ) {
			@list = @{$incoming_docs{$doc}};
		} else {
			croak "Wrong type of reference"
		}
		foreach my $term ( @list  ){
			$self->{'term_count'}{$term}++;
		}
	}


	$self->{'doc_count'} += scalar @doc_list;


	# Set edges in the graph
	foreach my $doc ( @doc_list ) {
		# Every word in this document
		my @edges;
		my $dnode = 'D:'.$doc;
		my $type = ref( $incoming_docs{$doc} ); 
		my @list = ( $type eq 'HASH' ?
					 keys %{ $incoming_docs{$doc} } :
					 @{ $incoming_docs{$doc} }
					);
					
		foreach my $term ( @list ) {		

			my $tnode = 'T:'.$term;

			my $lcount = ( $type eq 'HASH' ?
						   $incoming_docs{$doc}{$term} :
						   1);
						   
			my $l_weight = log( $lcount ) + 1;

			# We calculate global weight in the loop to conserve memory
			my $global_count = $self->{'term_count'}{$term};
			my $g_weight = log( $self->{'doc_count'} /  $global_count ) + 1;

			# edge weight is local weight * global weight
			my $t_weight = ( $g_weight * $l_weight );
			#print "Calculated edge $dnode -> $tnode with weight $t_weight", "\n"
				# if $self->{debug};
			push @edges, [ $dnode, $tnode, $t_weight ];
		}

		# Normalize the edges around each document so they add up to
		# unit weight
		my $sum;
		$sum += $_->[2] foreach @edges;
		$_->[2]/= $sum foreach @edges;

		
		#### XS VERSION ###############
		if ( $self->{'xs'} ) {
			my $map = $self->{'node_map'};
			foreach my $e ( @edges ) {
				my ( $doc, $term, $edge ) = @{$e};
				my $dindex = ( exists $map->{$doc}  
								? $map->{$doc}  
								: $self->_add_node( $doc, 2 ) );
								
				my $tindex =  ( exists $map->{$term} 
								? $map->{$term}
								: $self->_add_node( $term, 1 ));
								
				$self->{Graph}->set_edge( $dindex, $tindex, $edge );
			}
		
		#### PURE PERL VERSION ########
		} else {
			foreach my $e ( @edges ) {
				my ( $doc, $term, $edge ) = @{$e};
				print "$doc $term edge weight is ", $edge, "\n" if $self->{debug};
				$self->set_edge( $doc, $term, $edge );
			}
		}
		
	}

}


sub doc_count {
	$_[0]->{'doc_count'};
}

sub term_count {
	scalar $_[0]->dump_words();
}

sub retrieve {
	my ( $self, $file ) = @_;
	Storable::retrieve( $file );
}


sub dump_words {

	my ( $self ) = @_;
	return keys %{ $self->{'term_count'} };
}

=item search @QUERY

Searches the graph for all of the words in @QUERY.  Use find_similar if you
want to do a document similarity instead, or mixed_search if you want
to search on any combination of words and documents.  Returns a pair of hashrefs:
the first a reference to a hash of docs and relevance values, the second to 
a hash of words and relevance values.

=cut

sub search {
	my ( $self, @query ) = @_;	
	my @nodes = $self->_nodeify( 'T', @query );
	my $results = $self->raw_search( @nodes );	
	my ($docs, $words) = _partition( $results );
	return ( $docs, $words);
}


=item find_similar @DOCS

Given an array of document identifiers, performs a similarity search 
and  returns a pair of hashrefs. First hashref is to a hash of docs and relevance
 values, second is to a hash of words and relevance values.

=cut

sub find_similar {
	my ( $self, @docs ) = @_;
	my @nodes = $self->_nodeify( 'D', @docs );
	my $results = $self->raw_search( @nodes );
	my ($docs, $words) = _partition( $results );
	return ( $docs, $words);
}


=item mixed_search @DOCS

Given a hashref in the form:
    { docs => [ 'Title 1', 'Title 2' ],
      terms => ['buffalo', 'fox' ], }
     }
Runs a combined search on the terms and documents provided, and
returns a pair of hashrefs.  The first hashref is to a hash of docs
and relevance values, second is to a hash of words and relevance values.

=cut

sub mixed_search {
	my ( $self, $incoming ) = @_;

	croak "must provide hash ref to mixed_search method"
		unless defined $incoming &&
		ref( $incoming ) &&
		ref( $incoming ) eq 'HASH';

	my $tref = $incoming->{'terms'} || [];
	my $dref = $incoming->{'docs'}  || [];

	my @dnodes = $self->_nodeify( 'D', @{$dref} );
	my @tnodes = $self->_nodeify( 'T', @{$tref} );

	my $results = $self->raw_search( @dnodes, @tnodes );
	my ($docs, $words) = _partition( $results );
	return ( $docs, $words);
}


=item store FILENAME

Stores the object to a file for later use.  Not compatible (yet)
with compiled XS version, which will give a fatal error.

=cut

sub store {
	my ( $self, @args ) = @_;
	if ( $self->{'xs'} ) {
		croak "Cannot store object when running in XS mode.";
	} else {
		$self->SUPER::store(@args);
	}
}


# Partition - internal method.
# Takes a result set and splits it into two hashrefs - one for
# words and one for documents

sub _partition {
	my ( $e ) = @_;
	my ( $docs, $words );
	foreach my $k ( sort { $e->{$b} <=> $e->{$a} }
					keys %{ $e } ) {

		(my $name = $k ) =~ s/^[DT]://o;
		$k =~ /^D:/  ? 
			$docs->{$name} = $e->{$k}  :
			$words->{$name} = $e->{$k} ;
	}
	return ( $docs, $words );
}

sub _nodeify {
	my ( $self, $prefix, @list ) = @_;
	my @nodes;
	foreach my $item ( @list ) {
		my $name = $prefix.':'.$item;
		warn "Node $name not found"
			unless $self->{'xs'} 
			or defined $self->{'neighbors'}{$name};
		push @nodes, $name;
	}
	return @nodes;
}



sub degree {
	my ($self, $node ) = @_;
	if ( exists ( $self->{'neighbors'}{$node} ) ) {
		return scalar keys %{ $self->{'neighbors'}{$node} };
	}
}



sub set_edge {
	my ( $self, $source, $sink, $value ) = @_;
	croak "No source node" unless defined $source;
	croak "No sink node" unless defined $sink;
	croak "no value defined " unless defined $value;

	if ( $value > 1 ) {
		croak "found edge exceeding unit weight\n";
	}
	
	if ( defined $self->{'neighbors'}{$source}{$sink} or
		 defined $self->{'neighbors'}{$sink}{$source} ){
		 	die "Found existing edge ( $sink, $source )";
		 }
		
	$self->{'neighbors'}{$source}{$sink} = $value;
	$self->{'neighbors'}{$sink}{$source} = $value;
}


# Wipe the graph free of stored energies

sub _clear {
	my ( $self ) = @_;
	$self->{'energy'} = undef;
}

# Gather the stored energy values from the graph

sub _collect {
	my ( $self ) = @_;
	my $e = $self->{'energy'};
	my $result = {};
	foreach my $k ( keys %{$self->{'energy'}} ) {
		next unless $e->{$k} > $self->{'COLLECT_THRESHOLD'};
		$result->{$k} = $e->{$k};
	}
	return $result;
}

 #  Assign a starting energy ENERGY to NODE, and recursively distribute  the 
 #  energy to neighbor nodes.   Singleton nodes get special treatment 



sub _energize {

	my ( $self, $node, $energy ) = @_;


	return unless defined $self->{'neighbors'}{$node};
	my $orig = $self->{'energy'}{$node} || 0;
	$self->{'energy'}->{$node} += $energy;
	$self->{'depth'}++;

	if ( $self->{'debug'} > 1 ) {
		print '   ' x $self->{'depth'};
		print "$node: energizing  $orig + $energy\n";
	}
	#sleep 1;
	my $degree = scalar keys %{ $self->{'neighbors'}->{$node} };



	croak "Fatal error: encountered node of degree zero" unless $degree;
	my $subenergy = $energy / (log($degree)+1);


	# At singleton nodes (words that appear in only one document, for example)
	# Don't spread energy any further.  This avoids a "reflection" back and
	# forth from singleton nodes to their neighbors.

	if ( $degree == 1 and  $energy < $self->{'START_ENERGY'} ) {

		#do nothing
	
	} elsif ( $subenergy > $self->{ACTIVATE_THRESHOLD} ) {
		print '   ' x $self->{'depth'}, 
		"$node: propagating subenergy $subenergy to $degree neighbors\n"
		 if $self->{'debug'} > 1;
		foreach my $neighbor ( keys %{ $self->{'neighbors'}{$node} } ) {
			my $edge = $self->{'neighbors'}{$node}{$neighbor};
			my $weighted_energy = $subenergy * $edge;
			print '   ' x $self->{'depth'}, 
			" edge $edge ($node, $neighbor)\n"
				if $self->{'debug'} > 1;
			$self->_energize( $neighbor, $weighted_energy );
		} 
	}	
	$self->{'depth'}--;	
	return 1;
}


sub get_neighbors {
	my ( $self, $node ) = @_;
	my @list;
	foreach my $n ( keys %{ $self->{'neighbors'}{$node} } ){
		push @list, [$n, $self->{'neighbors'}{$node}{$n}];
	}
	return @list;
}


sub DESTROY {
	undef $_[0]->{Graph}
}

1;

=back

=head1 BUGS

=over

=item * Document-document links are not yet implemented

=item * No way to delete nodes once they're in the graph

=item * No way to break edges once they're in the graph

=item * Can't store graph if using compiled C internals

=back

=head1 AUTHOR 

Maciej Ceglowski E<lt>maciej@ceglowski.comE<gt>

The technique used here was developed in 2003 by John Cuadrado, and later
found to have antecedents in the spreading activation approach described 
in a 1981 doctoral dissertation by Scott Preece.
XS implementation thanks to Schuyler Erle.

=head1 CONTRIBUTORS 
	
	Schuyler Erle
	Ken Williams
	Leon Brocard  

=head1 COPYRIGHT AND LICENSE

Perl module:
(C) 2003 Maciej Ceglowski

XS Implementation:
(C) 2003 Maciej Ceglowski, Schuyler Erle

This program is free software, distributed under the GNU Public License.
See LICENSE for details.


=cut
