package Search::ContextGraph;

use strict;
use warnings;
use Carp;

our $VERSION = '0.01';


=head1 NAME

Search::ContextGraph - Run searches using a contextual network graph

=head1 SYNOPSIS

  use Search::ContextGraph;
  
  my $cg = Search::ContextGraph->new();
  $cg->load( 'file.tdm' );
  
  my %results = $cg->search( 'd1', 't34', 't12' );
  

=head1 DESCRIPTION

Search a document collection using a spreading activation search.   The search
algorithm represents the collection as a set of term and document nodes,
connected to one another based on a co-occurrence matrix.  If a word occurs in
a document, we create an edge between the appropriate term and document node.
Searches take place by spreading energy from a query node along the edges of 
the graph according to some simple rules.  All result nodes exceeding a threshold T are returned.   You can read a full description of this algorithm
at L<http://www.nitle.org/papers/Contextual_Network_Graph.pdf>.

The search engine gives expanded recall (relevant results even when there is no
keyword match) without incurring the kind of computational and patent issues
inflicted by latent semantic indexing (LSI).

=head1 METHODS

=over

=item new %PARAMS

Object constructor.   Parameters include:

=over

=item debug 

Turns verbose mode on when true

=item energy

initial starting energy, default 10000

=item threshold

Cutoff value for propagating energy to neighbor nodes

=back

=cut

sub new {
	my ( $class, %params) = @_;
	bless 
		{ debug => 0,
		  initial_energy => 10000,
		  threshold => 1,
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

sub get_initial_energy { $_[0]->{'initial_energy'} }
sub set_initial_energy { $_[0]->{'initial_energy'} = $_[1] }


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
	$self->_read_tdm( $file );
	$self->{'loaded'} = 1;
}


=item search @NODES

Given a list of nodes, returns a hash of nearest nodes with relevance values,
in the format NODE => RELEVANCE, for all nodes above the threshold value.  Term nodes are prefixed by 't', document nodes are prefixed by 'd'.  It's your job
to keep some kind of node index to value map handy.

=cut

sub search {
	my ( $self, @query ) = @_;
	
	foreach ( @query ) {
		$self->_energize( $_, $self->{'initial_energy'} );
	}
	my $results_href = $self->_collect();
	$self->_reset() or croak "Unable to clear graph energies";
	return $results_href;
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


# Wipe the graph free of stored energies

sub _reset {
	my ( $self ) = @_;
	$self->{'energy'} = undef;
	return 1;
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

	if ( $self->{'debug'} ) {
		print ' ' x $self->{'indent'};
		print "Energizing node $node with energy $energy\n";
	}
	#sleep 1;
	my $degree = scalar keys %{ $self->{'neighbors'}->{$node} };
	
	if ( $self->{'debug'} ) {
		print  ' ' x $self->{'indent'};
		print "Node $node has $degree neighbors\n" if $self->{'debug'}	
	}
	
	croak "Fatal error: encountered node of degree zero" unless $degree;
	my $subenergy = $energy / $degree;
	
	if ( $subenergy > $self->{'threshold'} ) {
		foreach my $neighbor ( keys %{ $self->{'neighbors'}{$node} } ) {
			my $weighted_energy = $subenergy * $self->{'neighbors'}{$node}{$neighbor};
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
