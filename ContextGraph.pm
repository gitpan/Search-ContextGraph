package Search::ContextGraph;

use strict;
use warnings;
use Carp;
use base "Storable";


our $VERSION = '0.05';


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

Object constructor.  

=cut

sub new {
	my ( $class, %params) = @_;
	bless 
		{ debug => 0,
		  START_ENERGY => 100,
		  ACTIVATE_THRESHOLD => 1,
		  COLLECT_THRESHOLD => 1,
	      %params,
	      neighbors => {}, 

		}, 
	$class;
}

=item [get|set]_activate_threshold

Accessor for node activation threshold value.  This value determines how far 
energy can spread in the graph

=cut

sub get_activate_threshold {    $_[0]->{'ACTIVATE_THRESHOLD'} }
sub set_activate_threshold {	$_[0]->{'ACTIVATE_THRESHOLD'} = $_[1] }


=item [get|set]_collect_threshold

Accessor for collection threshold value.  This determines how much energy a
node must have to make it into the result set.

=cut

sub get_collect_threshold {  $_[0]->{'COLLECT_THRESHOLD'} }
sub set_collect_threshold {	 $_[0]->{'COLLECT_THRESHOLD'} = $_[1] }


=item [get|set]_initial_energy

Accessor for initial energy value at the query node.  The higher this value,
the larger the result set

=cut

sub get_initial_energy { $_[0]->{'START_ENERGY'} }
sub set_initial_energy { $_[0]->{'START_ENERGY'} = $_[1] }


=item load_from_tdm TDM_FILE [, LM_FILE]

Opens and loads a term-document matrix (TDM) file to initialize the graph.
Optionally also opens and loads a document link matrix (DLM) file of document-to-document
links.  The TDM encodes information about term-to-document links, while the DLM
file holds information about inter-document links, like hyperlinks or citation data.
For notes on these file formats, see the README file
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
(You probably want one of search, find_similar, or mixed_search instead).

=cut

sub raw_search {
	my ( $self, @query ) = @_;

	$self->_clear();
	foreach ( @query ) {
		$self->_energize( $_, $self->{'START_ENERGY'} );
	}
	my $results_ref = $self->_collect();

	return $results_ref;
}

=item set_debug_mode

Turns verbose comments on if given a true value as its argument

=cut

sub set_debug_mode { $_[0]->{'debug'} = $_[1] }



# Opens and reads a term-document matrix (TDM) file.  The format for this file
# is described in the README


sub _read_tdm {
	my ( $self, $file ) = @_;
	print "Loading TDM...\n" if $self->{'debug'};


	open my $fh, $file or croak "Could not open $file: $!";
	for ( 1..4 ){
		my $skip = <$fh>;
	}
	my %neighbors;
	my $doc = 0;
	while (<$fh>) {
		chomp;
		my ( $count, %vals ) = split;
		while ( my ( $n, $v ) = each %vals ) {
			$neighbors{"D:$doc"}{"T:$n"} = $v;
			$neighbors{"T:$n"}{"D:$doc"} = $v;
		}
		$doc++;
	}
	$self->{'neighbors'} = \%neighbors;
	print "Loaded.\n" if $self->{'debug'};
	$self->{'from_TDM'} = 1;
	$self->{'doc_count'} = $doc;
}


=item add_documents %DOCS

Load up the search engine with documents in the form
TITLE => WORDHASH, where WORDHASH is a reference to a hash of terms
and occurence counts.  In other words,

	TITLE => { WORD1 => COUNT1, WORD2 => COUNT2 ... }


=cut

sub add_documents {

	my ( $self, %incoming_docs ) = @_;

	my @doc_list = keys %incoming_docs; # Make sure these remain in same order

	# Add word and document nodes to the graph
	foreach my $doc ( @doc_list ) {
		my $dnode = 'D:'.$doc;
		croak "Tried to add document with duplicate title: \"$doc\"\n"
			if exists( $self->{'neighbors'}{$dnode} );

	}


	$self->{'doc_count'} += scalar @doc_list;


	# Set edges in the graph
	foreach my $doc ( @doc_list ) {
		# Every word in this document
		my @edges;
		my $dnode = 'D:'.$doc;

		foreach my $term ( keys %{ $incoming_docs{$doc} } ) {		

			my $tnode = 'T:'.$term;

			my $lcount = $incoming_docs{$doc}{$term};
			my $l_weight = log( $lcount ) + 1;

			# We calculate global weight in the loop to conserve memory
			my $global_count = $self->degree( $tnode ) + 1;
			my $g_weight = log( $self->{'doc_count'} /  $global_count ) + 1;

			# edge weight is local weight * global weight
			my $t_weight = ( $g_weight * $l_weight );
			print "Calculated edge $dnode -> $tnode with weight $t_weight", "\n"
				 if $self->{debug};
			push @edges, [ $dnode, $tnode, $t_weight ];
		}

		# Normalize the edges around each document so they add up to
		# unit weight
		my $sum;
		$sum += $_->[2] foreach @edges;
		$_->[2]/= $sum foreach @edges;

		foreach my $e ( @edges ) {
			print "final weight is ", $e->[2], "\n" if $self->{debug};
			$self->set_edge( @{$e} );
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
	my %words;
	foreach my $n ( keys %{ $self->{'neighbors'} } ) {
		$words{$n}++ if $n =~ s/^T://o;
	}

	return  keys %words;
}

=item search @QUERY

Searches the graph for all of the words in @QUERY.   No support yet for 
document similarity searches, but it's coming.  Returns a pair of hashrefs:
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

Given an array of document identifiers, returns a pair of hashrefs.
First hashref is to a hash of docs and relevance values, second
is to a hash of words and relevance values.

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
		croak "Node $name not found"
			unless defined $self->{'neighbors'}{$name};
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
	return $self->{'energy'};
}

 #  Assign a starting energy ENERGY to NODE, and recursively distribute  the 
 #  energy to neighbor nodes.   Singleton nodes get special treatment 


sub _energize {

	my ( $self, $node, $energy ) = @_;


	return unless defined $self->{'neighbors'}{$node};

	$self->{'energy'}->{$node} += $energy;
	$self->{'indent'}++;

	if ( $self->{'debug'} > 1 ) {
		print ' ' x $self->{'indent'};
		print "Energizing node $node with energy $energy\n";
	}
	#sleep 1;
	my $degree = scalar keys %{ $self->{'neighbors'}->{$node} };

	if ( defined $self->{'debug'} && $self->{'debug'} > 1) {
		print  ' ' x $self->{'indent'};
		print "Node $node has $degree neighbors\n" if $self->{'debug'}	
	}

	croak "Fatal error: encountered node of degree zero" unless $degree;
	my $subenergy = $energy / $degree;


	# At singleton nodes (words that appear in only one document, for example)
	# Don't spread energy any further.  This avoids a "reflection" back and
	# forth from singleton nodes to their neighbors.

	if ( $degree == 1 and  $energy < $self->{'START_ENERGY'} ) {

		#do nothing


	# If the search starts on a singleton, we want to make sure the energy
	# gets passed along to the neighbor node, no matter how puny the edge weight
	# connecting the singleton term to its neighbor.  We diminish it a little bit,
	# just in case the neighbor is also a degree 1 node - otherwise the
	# energy would  bounce back and forth forever

	} elsif ( $degree == 1 ) {
		my ($neighbor) = keys %{ $self->{'neighbors'}{$node} };
		$self->_energize( $neighbor, $self->{'START_ENERGY'} * .9 );


	# 
	} elsif ( $subenergy > $self->{ACTIVATE_THRESHOLD} ) {
		print "\tsubenergy is $subenergy\n" if $self->{'debug'} > 1;
		foreach my $neighbor ( keys %{ $self->{'neighbors'}{$node} } ) {
			my $edge = $self->{'neighbors'}{$node}{$neighbor};
			my $weighted_energy = $subenergy * $edge;
			$self->_energize( $neighbor, $weighted_energy );
		} 
	}	
	$self->{'indent'}--;	
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

1;

=back

=head1 BUGS

=over

=item Document-document links are not yet implemented

=item No way to delete nodes once they're in the graph

=item No way to break edges once they're in the graph

=back

=head1 AUTHOR 

Maciej Ceglowski E<lt>maciej@ceglowski.comE<gt>

The technique used here was developed in 2003 by John Cuadrado, and later
found to have antecedents in the spreading activation approach described 
in a 1981 doctoral dissertation by Scott Preece.

=head1 CONTRIBUTORS 

Ken Williams
Leon Brocard

=head1 COPYRIGHT AND LICENSE

(C) 2003 Maciej Ceglowski

This program may be distributed under the same terms as Perl itself.


=cut
