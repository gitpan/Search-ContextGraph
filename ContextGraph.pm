package Search::ContextGraph;

use strict;
use warnings;
use Carp;
use base "Storable";
use File::Find;
use IO::Socket;

our $VERSION = '0.11';

# If you are in an environment that can't compile XS
# modules, comment out the next three lines of code

#################################
# XS is disabled in this version
#################################

#use DynaLoader;
#our @ISA = qw/DynaLoader Storable/;
#bootstrap Search::ContextGraph $VERSION;



my $count = 0;


=head1 NAME

Search::ContextGraph - spreading activation search engine

=head1 SYNOPSIS

  use Search::ContextGraph;

  my $cg = Search::ContextGraph->new();

  my %docs = (
    'first'  => [ 'elephant', 'snake' ],
    'second' => [ 'camel', 'pony' ],
    'third'  => { 'snake' => 2, 'constrictor' => 1 },
  );

  $cg->bulk_add( %docs );

	- OR -

  my $cg = Search::ContextGraph->load_from_dir( "./myfiles" );

  # Lazy programmer's search

  my @ranked_docs = $cg->simple_search( 'peanuts' );


  # Spreading activation can give you 
  # both similar documents and semantically 
  # related words

  my ( $docs, $words ) = $cg->search('snake');


  # You can use a document as your query, instead
  # of searching on a keyword

  my ( $docs, $words ) = $cg->find_similar('First Document');


  # Or you can combine words and docs in your
  # query for maximum control

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

Spreading activation is a neat technique for building search engines that 
return accurate results for a query even when there is no exact keyword match.
The engine works by building a data structure called a B<context graph>, which
is a giant network of document and term nodes.  All document nodes are connected
to the terms that occur in that document; similarly, every term node is connected
to all of the document nodes that term occurs in.   We search the graph by 
starting at a query node and distributing a set amount of energy to its neighbor
nodes.  Then we recurse, diminishing the energy at each stage, until this
spreading energy falls below a given threshold.   Each node keeps track of 
accumulated energy, and this serves as our measure of relevance.  

This means that documents that have many words in common will appear similar to the
search engine.  Likewise, words that occur together in many documents will be 
perceived as semantically related.  Especially with larger, coherent document 
collections, the search engine can be quite effective at recognizing synonyms
and finding useful relationships between documents. You can read a full 
description of the algorithm at L<http://www.nitle.org/papers/Contextual_Network_Graphs.pdf>.

The search engine gives expanded recall (relevant results even when there is no
keyword match) without incurring the kind of computational and patent issues
posed by latent semantic indexing (LSI).  The technique used here was originally
described in a 1981 dissertation by Scott Preece.

=head1 CONSTRUCTORS

=over

=item new %PARAMS

Object constructor.   Possible parameters:

=over

=item auto_reweight

Rebalance the graph every time a change occurs. Default is true.
Disable and do by hand using L<reweight_graph> for better performance in
graphs with frequent updates/additions/deletions.


=item debug LEVEL

Set this to 1 or 2 to turn on verbose debugging output

=item max_depth

Set the maximum distance to spread energy out from the start
node.  Default is effectively unlimited.  You can tweak it using L<set_max_depth>.
Comes in handy if you find searches are too slow.

=item xs

When true, tells the module to use compiled C internals.  This reduces
memory requirements by about 60%, but actually runs a little slower than the 
pure Perl version.  Don't bother to turn it on unless you have a huge graph. 
Default is pure Perl.

=over

=item * using the compiled version makes it impossible to store the graph to disk.

=item * xs is B<broken> in version 0.09.   But it will return in triumph!

=back 

=item START_ENERGY

Initial energy to assign to a query node.  Default is 100.

=item ACTIVATE_THRESHOLD

Minimal energy needed to propagate search along the graph.  Default is 1.

=item COLLECT_THRESHOLD

Minimal energy needed for a node to enter the result set.  Default is 1.

=back

=cut


sub new {
	my ( $class, %params) = @_;

	# backwards compatible...
	*add_document = \&add;
	*add_documents = \&bulk_add;

	my $obj = bless 
		{ debug => 0,
		  auto_reweight => 1,
		  use_global_weights => 1,
		  max_depth => 100000000,
		  START_ENERGY => 100,
		  ACTIVATE_THRESHOLD => 1,
		  COLLECT_THRESHOLD => .2,
	      %params,

	      depth => 0,
	      neighbors => {}, 

		}, 
	$class;
	if ( $obj->{'xs'} ) {
		croak "XS not implemented in this version";

=item
		my $graph = Search::ContextGraph::Graph->new(
			$obj->{START_ENERGY},
			$obj->{ACTIVATE_THRESHOLD},
			$obj->{COLLECT_THRESHOLD},
		);
		$obj->{Graph} = $graph;
		$obj->{'next_free_id'} = 0;
		$obj->{'node_map'} = {};

=cut

	}

	return $obj;

}


=item load_from_dir  DIR [, \&PARSE ]

Load documents from a directory.  Takes two arguments, a directory path
and an optional parsing subroutine.  If the parsing subroutine is passed
an argument, it will use it to extract term tokens from the file.
By default, the file is split on whitespace and stripped of numbers and
punctuation.

=cut

{
	my $parse_sub;

	sub load_from_dir  {
		my ( $class, $dir, $code ) = @_;

		croak "$dir is not a directory" unless -d $dir;

		require File::Find;
		unless ( defined $code 
				 and ref $code 
				 and ref $code eq 'CODE' ) {
			$code = sub {
				my $text = shift;
				$text =~ s/[^\w]/ /gs;
				my @toks = split /\s+/m, $text;
				return grep { length($_) > 1 } @toks;
			};
		}

		$parse_sub = $code;
		my %docs;

		# Recursively open every file and provide the contents
		# to whatever parsing subroutine we're using

		my $reader = 

			sub {
				my ( $parse ) = @_;
				return if /^\./;
				return unless -f $_;
				open my $fh, $_ or 
				   croak "Could not open file $File::Find::name: $!";
				local $/;
				my $contents = <$fh>;
				close $fh or croak "failed to close filehandle";
				my @words = $parse_sub->($contents);
				$docs{ $File::Find::name } = \@words;
			};


		find( $reader , $dir );
		my $self = __PACKAGE__->new();
		$self->bulk_add( %docs );
		return $self;
	}
}



=item load_from_tdm FILENAME

Opens and loads a term-document matrix (TDM) file to initialize the graph.
The TDM encodes information about term-to-document links.
This is a legacy method mainly for the convenience of the module author.
For notes on the proper file format, see the README file.
=cut

sub load_from_tdm {
	my  ( $self, $file ) = @_;
	croak "TDM file $file does not exist" unless -f $file;
	return if $self->{'loaded'};
	$self->_read_tdm( $file );
	$self->{'loaded'} = 1;
	$self->reweight_graph();
}


=item rename OLD, NEW

Renames a document.  Will return undef if the new name is already in use.

=cut
sub rename {

	my ( $self, $old, $new ) = @_;
	croak "rename method needs two arguments" unless
		defined $old and defined $new;
	croak "document $old not found" unless
		exists $self->{neighbors}{'D:'.$old};
	
	return if exists $self->{neighbors}{'D:'.$new};
	


}



=item retrieve FILENAME

Loads a previously stored graph from disk, using Storable.

=cut

sub retrieve {
	my ( $self, $file ) = @_;
	croak "Must provide a filename to retrieve graph"
		unless  $file;
	croak "'$file' is not a file" unless
		-f $file;

	Storable::retrieve( $file );
}


=back

=head1 ACCESSORS

=over

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


=item [get|set]_auto_reweight

Accessor for auto reweight flag.  If true, edge weights will be recalculated
every time a document is added, updated or removed.  This can significantly slow 
down large graphs.  On by default.

=cut

sub get_auto_reweight{ $_[0]->{auto_reweight} }
sub set_auto_reweight{ $_[0]->{auto_reweight} = $_[0]->[1]; }


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

=item [get|set]_debug_mode LEVEL

Turns debugging on or off.  1 is verbose, 2 is very verbose, 0 is off.

=cut

sub get_debug_mode { $_[0]->{debug} }
sub set_debug_mode {
	my ( $self, $mode ) = @_;
	$self->{'debug'} = $mode;
}



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

=item [get|set]_max_depth LEVEL

You can tell the graph to cut off searches after a certain distance from
the query node.  This can speed up searches on very large graphs, and has
little adverse effect, especially if you are interested in just the first
few search results.  Set this value to undef to restore the default (10^8).

=cut

sub get_max_depth { $_[0]->{max_depth} }
sub set_max_depth { croak "Tried to set maximum depth to an undefined value" 
	 unless defined $_[1];
	 $_[0]->{max_depth} = $_[1] || 100000000 
}




=back

=head1 METHODS

=over 

=item add DOC, WORDS

Add a document to the search engine.  Takes as arguments a unique doc
identifier and a reference to an array or hash of words in the 
document.
For example:

	TITLE => { WORD1 => COUNT1, WORD2 => COUNT2 ... }

or

	TITLE => [ WORD1, WORD2, WORD3 ]

Use L<bulk_add> if you want to pass in a bunch of docs all at once.

=cut


sub add {

	my ( $self, $title, $words ) = @_;


	croak "Please provide a word list" unless defined $words;
	croak "Word list is not a reference to an array or hash"
		unless ref $words and ref $words eq "HASH" or ref $words eq "ARRAY";

	croak "Please provide a document identifier" unless defined $title;

	my $dnode = 'D:'.$title;
	croak "Tried to add document with duplicate identifier: '$title'\n"
		if exists $self->{neighbors}{$dnode};

	my @list;
	if ( ref $words eq 'ARRAY' ) {
		@list = @{$words};
	} else {
		@list = keys %{$words};
	}


	croak "Tried to add a document with no content" unless scalar @list;

	my @edges;
	foreach my $term ( @list ) {
		my $tnode = 'T:'.lc( $term );

		# Local weight for the document
		my $lcount = ( ref $words eq 'HASH' ? $words->{$term} : 1 );

		# Update number of docs this word occurs in
		my $gcount = ++$self->{term_count}{lc( $term )};

		my $final_weight = 1;
		push @edges, [ $dnode, $tnode, $final_weight, $lcount ];

	}

	__normalize( \@edges );

	# XS VERSION 

	if ( $self->{xs} ) {
		my $map = $self->{'node_map'};
		foreach my $ed ( @edges ) {
			my ( $d, $t, $e ) = @{$ed};
			my $dindex = ( exists $map->{$d}  
							? $map->{$d}  
							: $self->_add_node( $d, 2 ) );

			my $tindex =  ( exists $map->{$t} 
							? $map->{$t}
							: $self->_add_node( $t, 1 ));

			$self->{Graph}->set_edge( $dindex, $tindex, $ed );
		}


	# PURE PERL VERSION 

	} else {
		foreach my $e ( @edges ) {
			$self->{neighbors}{$e->[0]}{$e->[1]} = join ',', $e->[2], $e->[3];
			$self->{neighbors}{$e->[1]}{$e->[0]} = join ',', $e->[2], $e->[3];
		}

	}
	#print "Reweighting graph\n";
	$self->reweight_graph() if $self->{auto_reweight};
	return 1;

}


=item add_file PATH [, name => NAME, parse => CODE]

Adds a document from a file.   By default, uses the PATH provided as the document
identifier, and parses the file by splitting on whitespace.  If a fancier title, 
or more elegant parsing behavior is desired, pass in named arguments as indicated.
NAME can be any string, CODE should be a reference to a subroutine that takes one
argument (the contents of the file) and returns an array of tokens, or a hash in the
form TOKEN => COUNT, or a reference to the same.

=cut

sub add_file {
	my ( $self, $path, %params ) = @_;
	
	croak "Invalid file '$path' provided to add_file method."
		unless defined $path and -f $path;
		
	my $title = ( exists $params{name} ? $params{name} : $path );

	local $/;
	open my $fh, $path or croak "Unable to open $path: $!";
	my $content = <$fh>;
	
	my $ref;
	
	if ( exists $params{parse} ) {
		croak "code provided is not a reference" unless
			ref $params{parse};
		croak "code provided is not a subroutine" unless
			ref $params{parse} eq 'CODE';
		
		$ref = $params{parse}->( $content );
		croak "did not get an appropriate reference back after parsing"
			unless ref $ref and ref $ref =~ /(HASH|ARRAY)/;
		
		
	} else {
	
		my $code = sub { 
			my $txt  = shift; 
			$txt =~ s/\W/ /g;
			my @toks = split m/\s+/, $txt;
			\@toks;
		};
		$ref = $code->($content);
	}
	
	return unless $ref;
	$self->add( $title, $ref );
	
}

=item bulk_add DOCS

Add documents to the graph in bulk.  Takes as an argument a hash
whose keys are document identifiers, and values are references
to hashes in the form { WORD1 => COUNT, WORD2 => COUNT...}
This method is faster than adding in documents one by one if
you have auto_rebalance turned on.

=cut

sub bulk_add {

	my ( $self, %incoming_docs ) = @_;



	# Disable graph rebalancing until we've added everything
	{
		local $self->{auto_reweight} = 0;

		foreach my $doc ( keys %incoming_docs ) {
			$self->add( $doc, $incoming_docs{$doc});
		} 
	}
	$self->reweight_graph() if $self->{auto_reweight};
}


sub degree { scalar keys %{$_[0]->{neighbors}{$_[1]}} }

=item delete DOC

Remove a document from the graph.  Takes a document identifier
as an argument.  Returns 1 if successful, undef otherwise.

=cut

sub delete {

	my ( $self, $title ) = @_;
	return unless defined $title;
	my $node = 'D:'.$title;

	my $n = $self->{neighbors};
	return unless exists $self->{neighbors}{$node};

	my @terms = keys %{ $self->{neighbors}{$node} };

	# Check to see if we have orphaned any terms
	foreach my $t ( @terms ) {
		#delete $self->{neighbors}{$node}{$t};
		delete $self->{neighbors}{$t}{$node};

		if ( $self->doc_count( $t ) == 0	) {
			delete $self->{neighbors}{$t};
		} 
	}

	delete $self->{neighbors}{$node};
	$self->reweight_graph if $self->{auto_reweight};
}


=item has_doc DOC

Returns true if the document with identifier DOC is in the collection

=cut

sub has_doc { 
	my ( $self, $doc ) = @_;
	carp "Received undefined value for has_doc" unless defined $doc;
	my $node = 'D:'.$doc;
	return exists $self->{neighbors}{$node} ||  undef;
}

=item has_term TERM

Returns true if the term TERM is in the collection

=cut

sub has_term { 
	my ( $self, $term ) = @_;
	carp "Received undefined value for has_term" unless defined $term;
	my $node = 'T:'.$term;
	return exists $self->{neighbors}{$node} || undef;
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








=item reweight_graph

Iterates through the graph, calculating edge weights and normalizing 
around nodes.  This method is automatically called every time a 
document is added, removed, or updated, unless you turn the option
off with auto_reweight(0).   When adding a lot of docs, this can be
time consuming, so either set auto_reweight to off or use the 
L<bulk_add> method to add lots of docs at once

=cut

sub reweight_graph {
	my ( $self ) = @_;

	my $n = $self->{neighbors}; #shortcut
	my $doc_count = $self->doc_count();
	#print "Renormalizing for doc count $doc_count\n" if $self->{debug};
	foreach my $node ( keys %{$n} ) {

		next unless $node =~ /^D/o;
		my @terms = keys %{ $n->{$node} };
		my @edges;
		foreach my $t ( @terms ) {

			my $pair = $n->{$node}{$t};
			my ( undef, $lcount ) = split /,/, $pair;
			( my $term = $t ) =~ s/^T://;
			croak "did not receive a local count" unless $lcount;
			my $weight;
			if ( $self->{use_global_weights} ) {

				my $gweight = log( $doc_count / $self->doc_count( $term ) ) + 1;
				my $lweight = log( $lcount ) + 1;
				$weight = ( $gweight * $lweight );
			} else {

				$weight = log( $lcount ) + 1;
			}
			push @edges, [ $node, $t, $weight, $lcount ];
		}

		__normalize( \@edges );

		foreach my $e ( @edges ) {
			my $pair = join ',', $e->[2], $e->[3];
			$n->{$node}{$e->[1]} = $n->{$e->[1]}{$node} = $pair;
		}
	}
	return 1;
}




=item update ID, WORDS

Given a document identifier and a word list, updates the information for
that document in the graph.  Returns the number of changes made

=cut

sub update {

	my ( $self, $id, $words ) = @_;

	croak "update not implemented in XS" if $self->{xs};
	croak "Must provide a document identifier to update_document" unless defined $id;
	my $dnode = 'D:'.$id;

	return unless exists $self->{neighbors}{$dnode};
	croak "must provide a word list " unless defined $words and 
										     		ref $words and
										    	 	( ref $words eq 'HASH' or
										    	 	  ref $words eq 'ARRAY' );

	my $n = $self->{neighbors}{$dnode};
	# Get the current word list
	my @terms = keys %{ $n };


	if ( ref $words eq 'ARRAY' ) {
		my %words;
		$words{$_}++ foreach @$words;
		$words = \%words;
	}

	local $self->{auto_reweight} = 0;

	my $must_reweight = 0;
	my %seen;

	foreach my $term ( keys %{$words} ) {

		my $t = 'T:'.$term;

		if ( exists $n->{$t} ){

			# Update the local count, if necessary
			my $curr_val = $n->{$t};
			my ( undef, $loc ) = split m/,/, $curr_val;

			unless ( $loc == $words->{$term} ) {
				$n->{$t} = join ',', 1, $words->{$term};
				$must_reweight++;
			}	
			}

		else {

			$n->{$t} = 
				$self->{neighbors}{$t}{$dnode} = 
				join ',', 1, $words->{$term};
			$must_reweight++;
		}

		$seen{$t}++;
	}

	# Check for deleted words
	foreach my $t ( @terms ) {
		$must_reweight++ 
			unless exists $seen{$t};
	}

	$self->reweight_graph() if 
		$must_reweight;

	return $must_reweight;

}


=item doc_count [TERM]

Returns a count of all documents that TERM occurs in.
If no argument is provided, returns a document count
for the entire collection.

=cut

sub doc_count {
	my ( $self, $term ) = @_;
	if ( defined $term ) {
		$term = 'T:'.$term unless $term =~ /^T:/;
		my $node = $self->{neighbors}{$term};
		return 0 unless defined $node;
		return scalar keys %{$node};
	} else {
		return scalar grep /D:/, 
			keys %{ $self->{'neighbors'} };
	}
}


=item doc_list [TERM]

Returns a sorted list of document identifiers that contain
TERM, in ASCII-betical order.  If no argument is given,
returns a sorted document list for the whole collection.

=cut

sub doc_list {
	my ( $self, $term ) = @_;
	my $t;
	if ( defined $term and $term !~ /T:/) {
		$t = 'T:'.$term;
	}
	my $hash = ( defined $term ?
				 $self->{neighbors}{$t} :
				 $self->{neighbors} );

	sort map { s/^D://o; $_ }
		 grep /D:/, keys %{ $hash };
}


sub dump {
	my ( $self ) = @_;
	my @docs = $self->doc_list();

	foreach my $d ( @docs ) {
		print $self->dump_node( $d );
	}
}

=item dump_node NODE

Lists all of the neighbors of a node, together with edge
weights connecting to them

=cut

sub dump_node {
	my ( $self, $node ) = @_;

	my @lines;
	push @lines, join "\t", "COUNT", "WEIGHT", "NEIGHBOR";

	foreach my $n ( keys %{ $self->{neighbors}{$node} } ) {
		my $v = $self->{neighbors}{$node}{$n};
		my ( $weight, $count ) = split /,/, $v;
		push @lines, join "\t", $count, substr( $weight, 0, 8 ), $n;
	}
	return @lines;
}



=item dump_tdm [FILE]

Dumps internal state in term-document matrix (TDM) format, which looks 
like this:

	A B C B C B C
	A B C B C B C
	A B C B C B C

Where each row represents a document, A is the number of terms in the
document, B is the term node and C is the edge weight between the doc
node and B.   Mostly used as a legacy format by the module author. 
Doc and term nodes are printed in ASCII-betical sorted order, zero-based 
indexing.  Up to you to keep track of the ID => title mappings, neener-neener!
Use doc_list and term_list to get an equivalently sorted list

=cut

sub dump_tdm {
	my ( $self, $file ) = @_;

	my $counter = 0;
	my %lookup;
	$lookup{$_} = $counter++ foreach $self->term_list;

	my @docs = $self->doc_list;

	my $fh;
	if ( defined $file ) {
		open $fh, "> $file" or croak
			"Could not open TDM output file: $!";
	} else {
		*fh = *STDOUT;
	}
	foreach my $doc ( @docs ) {
		my $n = $self->{neighbors}{$doc};

		my $row_count = scalar keys %{$n};
		print $fh $row_count;

		foreach my $t ( sort keys %{$doc} ) {
			my $index = $lookup{$t};
			my ( $weight, undef ) = split m/,/, $n->{$t};
			print $fh ' ', $index, ' ', $weight;
		}
		print $fh "\n";
	}
}




=item term_count [DOC]

Returns the number of unique terms in a document or,
if no document is specified, in the entire collection.

=cut

sub term_count {
	my ( $self, $doc ) = @_;
	if ( defined $doc ) {
		my $node = $self->{neighbors}{'D:'.$doc};
		return 0 unless defined $node;
		return scalar keys %{$node};
	} else {
		return scalar grep /T:/, 
		keys %{ $self->{neighbors} };
	}
}


=item term_list [DOC]

Returns a sorted list of unique terms appearing in the document
with identifier DOC, in ASCII-betical order.  If no argument is
given, returns a sorted term list for the whole collection.

=cut

sub term_list {
	my ( $self, $doc ) = @_;

	my $node = ( defined $doc ?
				 $self->{neighbors}{'D:'.$doc} :
				 $self->{neighbors}
			 );

	sort map { s/^T://o; $_ }
		 grep /T:/, keys %{ $node };
}



=item word_count [TERM]

Returns the total occurence count for a term, or if no argument is given,
a word count for the entire collection.  The word count is always greater than
or equal to the term count.

=cut

sub word_count {

	my ( $self, $term ) = @_;

	my $n = $self->{neighbors}; # shortcut

	my $count = 0;
	my @terms;
	if ( defined $term ) {
		push @terms, $term;
	}	else {
		@terms = $self->term_list();
	}

	foreach my $term (@terms ) {
		$term = 'T:'.$term unless $term =~/^T:/o;
		foreach my $doc ( keys %{ $n->{$term} } ) {
			( undef, my $lcount ) = split /,/, $n->{$term}{$doc};
			$count += $lcount;
		}
	}

	return $count;
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



=item simple_search QUERY

This is the DWIM method - takes a query string as its argument, and returns an array
of documents, sorted by relevance.

=cut

sub simple_search {
	my ( $self, $query ) = @_;
	my @words = map { s/\W+//g; lc($_) }
				split m/\s+/, $query;	
	my @nodes = $self->_nodeify( 'T', @words );
	my $results = $self->raw_search( @nodes );
	my ($docs, $words) = _partition( $results );
	my @sorted_docs = sort { $docs->{$b} <=> $docs->{$a} } keys %{$docs};
	return @sorted_docs;
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

				$neighbors{$dnode}{$tnode} = $edge.',1';
				$neighbors{$tnode}{$dnode} = $edge.',1';
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



#
# 	INTERNAL METHODS
# 

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


	return unless defined $self->{neighbors}{$node};
	my $orig = $self->{energy}{$node} || 0;
	$self->{energy}->{$node} += $energy;
	return if ( $self->{depth} == $self->{max_depth} );
	$self->{depth}++;

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
			my $pair = $self->{'neighbors'}{$node}{$neighbor};
			my ( $edge, undef ) = split /,/, $pair;
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


# Given an array, normalize using cosine normalization

sub __normalize {
	my ( $arr ) = @_;

	croak "Must provide array ref to __normalize" unless
		defined $arr and
		ref $arr and
		ref $arr eq 'ARRAY';

	my $sum;
	$sum += $_->[2] foreach @{$arr};
	$_->[2]/= $sum foreach @{$arr};
	return 1;
}




sub DESTROY {
	undef $_[0]->{Graph}
}

1;

=back

=head1 BUGS

=over

=item * Document-document links are not yet implemented

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
