package Search::ContextGraph;

use strict;
use warnings;
use Carp;

our $VERSION = '0.02';


=head1 NAME

Search::ContextGraph - Run searches using a contextual network graph

=head1 SYNOPSIS

  use Search::ContextGraph;
  
  my %docs = ( 'First Document' => { 'elephant' => 2, 'snake' => 1 }
  			   'Other Document' => { 'snake' => 1, 'constrictor' => 1 }
  			   ...
  			  );
  
  my $cg = Search::ContextGraph->new();
  $cg->add_documents( %docs );
 
  my $results = $cg->search('snake');
  
  foreach my $k ( keys %{ $results } ) {
  		print "$k had relevance ", $results->{$k}, "\n";
  }
  
  	
  

=head1 DESCRIPTION

Search a document collection using a spreading activation search.   The search
algorithm represents the collection as a set of term and document nodes,
connected to one another based on a co-occurrence matrix.  If a word occurs in
a document, we create an edge between the appropriate term and document node.
Searches take place by spreading energy from a query node along the edges of 
the graph according to some simple rules.  All result nodes exceeding a threshold 
T are returned.   You can read a full description of this algorithm
at L<http://www.nitle.org/papers/Contextual_Network_Graph.pdf>.

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
		{ debug => 1,
		  START_ENERGY => 100,
		  ACTIVATE_THRESHOLD => 1,
		  COLLECT_THRESHOLD => 1,
	      %params 
		}, 
	$class;
}
	
=item [get|set]_threshold

Accessor for threshold value.  This value determines how far energy can spread
in the graph

=cut

sub get_threshold { $_[0]->{'threshold'} }
sub set_threshold {	$_[0]->{'threshold'} = $_[1] }


=item [get|set]_initial_energy

Accessor for initial energy value at the query node.  The higher this value,
the larger the result set

=cut

sub get_initial_energy { $_[0]->{'threshold'} }
sub set_initial_energy { $_[0]->{'energy'} = $_[1] }


=item load TDM_FILE [, LM_FILE]

Opens and loads a term-document matrix (TDM) file to initialize the graph.
Optionally also opens and loads a document link matrix (DLM) file of document-to-document
links.  The TDM encodes information about term-to-document links, while the DLM
file holds information about inter-document links, like hyperlinks or citation data.
For notes on these file formats, see the README file
Note that document-document links are NOT YET IMPLEMENTED.

=cut
sub load {
	my  ( $self, $file ) = @_;
	croak "TDM file $file does not exist" unless -f $file;
	return if $self->{'loaded'};
	$self->_read_tdm();
	$self->{'loaded'} = 1;
}


=item raw_search @NODES

Given a list of nodes, returns a hash of nearest nodes with relevance values,
in the format NODE => RELEVANCE, for all nodes above the threshold value

=cut

sub raw_search {
	my ( $self, @query );
	
	foreach ( @query ) {
		$self->energize( $_ );
	}
	my $results_ref = $self->_collect();
	$self->_reset();
	return %{ $results_ref };
}

=item set_debug_mode

Turns verbose comments on if given a true value as its argument

=cut

sub set_debug_mode { $_[0]->{'debug'} = $_[1] }


=item _read_tdm FILE

Opens and reads a term-document matrix (TDM) file.  The format for this file
is described in the README

=cut

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
			$neighbors{"d$doc"}{"t$n"} = $v;
			$neighbors{"t$n"}{"d$doc"} = $v;
		}
		$doc++;
	}
	$self->{'neighbors'} = \%neighbors;
	print "Loaded.\n" if $self->{'debug'};
}


=item add_documents %DOCS

Load up the search engine with documents in the form
TITLE => WORDHASH, where WORDHASH is a reference to a hash of terms
and occurence counts.  In other words,

	TITLE => { WORD1 => COUNT1, WORD2 => COUNT2 ... }
	

=cut

sub add_documents {

	my ( $self, %docs ) = @_;
	
	my %doc_lookup;
	my %term_lookup;
	
	my %doc_in;
	my %seen_words;
	my %doc_map;
	my %g_weights;  # global term weights
	
	my $doc_index = 0;
	my $term_index = 0;
	
	my @doc_list = keys %docs; # Make sure these remain in same order
	
	my $num_docs = scalar @doc_list;
	
	
	# First, parse the vocabulary out
	foreach my $doc ( @doc_list ) {
		$doc_lookup{ 'd'.$doc_index } = $doc;
		foreach my $term ( keys %{ $docs{$doc} } ) {
			$seen_words{$term}++;
			$doc_map{$term} = "d".$doc_index;
		}
		$doc_index++;
	}
	
	
	
	#Next, figure out who the singletons are,
	#and keep a mapping
	
	my %singletons;
	$term_index = 0;
	my $max_seen = 0;
	foreach my $seen ( keys %seen_words ) {
		if ( $seen_words{$seen} == 1 ) {
			$singletons{$seen} = $doc_map{$seen};
			delete $seen_words{$seen};
		} else {
			$term_lookup{$seen} = 't'.$term_index++;
			$max_seen = $seen_words{$seen} if 
				$seen_words{$seen} > $max_seen;
		} 
	}
	
	$self->{'singletons'} = \%singletons;
	$doc_index = 0;
	
	
	
	
	# Set edges in the graph
	foreach my $doc ( @doc_list ) {
		# Every word in this document
		
		foreach my $term ( keys %{ $docs{$doc} } ) {
			next unless $seen_words{$term};
			
			my $tindex = $term_lookup{$term};
			my $dindex = 'd'.$doc_index;
			my $lcount = $docs{$doc}{$term};
			my $l_weight = log( $lcount ) + 1;
		
			# We calculate global weight in the loop to conserve memory
			my $g_weight = log( $num_docs / $seen_words{$term} );
			
			# edge weight is local weight * global weight, normalized 
			# so it falls between zero and one
			
			my $t_weight = ( $g_weight * $l_weight ) / $max_seen;
			print $t_weight, "\n";
			$self->set_edge( $dindex, $tindex, $t_weight);
		}
		$doc_index++;
	}
	
	
	foreach my $single  ( keys %{ $self->{'singletons'} } ) {
		print $single, ' ', $self->{'singletons'}->{$single}, "\n";
	}
	
	$self->{'doc_lookup'} = \%doc_lookup;
	$self->{'term_lookup'} = \%term_lookup;
}
	
	
	
=item search @QUERY

Searches the graph for all of the words in @QUERY.   No support yet for 
document similarity searches, but it's coming.  Returns a hashref
to a hash of document titles and relevance values.

=cut
	
sub search {
	my ( $self, @query ) = @_;
	
	
	# First, check keyword search
	
	$self->_clear();
	foreach my $word ( @query ) {
		print "Searching $word \n" if $self->{'debug'};
		
		# Check to see if this is a singleton word
		
		if ( exists ( $self->{'singletons'}{$word} )){
			print "\t$word  is a singleton\n" if $self->{'debug'};
			my $dnode = $self->{'singletons'}{$word};
			$self->_energize( $dnode, $self->{'START_ENERGY'});
		
		} else {
			
			my $tnode = $self->{'term_lookup'}{$word}
				or carp "Word $word not found\n";
			next unless $term;
			$self->_energize( $tnode, $self->{'START_ENERGY'} );
		}
	}
	
	
	my $e = $self->{'energy'};
	my %result;
	foreach my $k ( sort { $e->{$b} <=> $e->{$a} }
					keys %{ $e } ) {
		
		next if $k =~ /^t/;   # skip term nodes
		my $doc = $self->{'doc_lookup'}->{$k};
		$result{$doc} = $e->{$k};
	}
	return \%result;
	
}


sub set_edge {
	my ( $self, $source, $sink, $value ) = @_;
	croak "No source node" unless defined $source;
	croak "No sink node" unless defined $sink;
	croak "no value defined " unless defined $value;
	
	$self->{'neighbors'}{$source}{$sink} = $value;
	$self->{'neighbors'}{$sink}{$source} = $value;
	#print "\tsetting edge $sink, $source, $value\n";
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

=item _energize NODE, ENERGY

Private method.   Assigns a starting energy ENERGY to NODE, and recursively distributes the energy to neighbor nodes.    

=cut

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
	print "\tsubenergy is $subenergy\n" if $self->{'debug'} > 1;
	if ( $subenergy > $self->{ACTIVATE_THRESHOLD} ) {
		foreach my $neighbor ( keys %{ $self->{'neighbors'}{$node} } ) {
			my $edge = $self->{'neighbors'}{$node}{$neighbor};
			my $weighted_energy = $subenergy * $edge;
			$self->_energize( $neighbor, $weighted_energy );
		} 
	}	
	$self->{'indent'}--;	
	return 1;
}


1;

=back

=head1 BUGS

=over

=item Document-document links are not yet implemented

=back

=head1 AUTHOR

Maciej Ceglowski E<lt>maciej@ceglowski.comE<gt>

=head1 COPYRIGHT AND LICENSE

(C) 2003 Maciej Ceglowski, John Cuadrado, NITLE

This program may be distributed under the same terms as Perl itself.


=cut
