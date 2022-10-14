/**
 * **********************************************
 * \file   densenauty.pl
 * \module densenauty
 * \brief  main module for nauty SWI-Prolog interface.
 * **********************************************
 * 
 * **********************************************
 * \author Michael Frank
 * \email  frankm@post.bgu.ac.il
 * **********************************************
 */
:- module(densenauty,
	[ densenauty/3,
	  densenauty/8,
	  canonic_graph/3,
	  canonic_graph/5,
	  canonic_graph/6,
	  canonic_graphs/3,
	  isomorphic_graphs/6,
	  graph_convert/5,
	  graph_convert_file/5,
	  graph_convert_file/6,
	  group_graphs_by_canonic/3,
	  group_graphs_by_canonic/4,
	  non_isomorphic/3,
	  non_isomorphic/4
	]).

/*
 * **********************************************
 * 
 * **********************************************
 */
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(shlib)).
:- use_module(library(option)).

:- multifile
	prolog:message//1.

/*
 * **********************************************
 * 
 * **********************************************
 */
:- load_foreign_library('pl-nauty.so',install).
:- initialization('$densenauty_initialize').

/*
 * **********************************************
 * densenauty(+NVert, +Graph, +Options)
 * densenauty(+NVert, +Graph, ?Labeling, ?Partition, ?Permutation, ?Orbits, ?CanonicGraph, +Options)
 * 
 * call nauty's densenauty function, which does
 * graph canonization.
 * 
 * options:
 * 	- infmt(fmt)
 * 	  format of input graph Graph.
 * 	  defaults to adj_matrix
 * 	
 * 	- cgfmt(fmt)
 * 	  format of canonic graph CanonicGraph
 * 	  defaults to adj_matrix
 * 	
 * 	- digraph(boolean)
 * 	  true if Graph is directed.
 * 	  default is false.
 * 	
 * 	- get_lab(Labeling)
 * 	  unify Labeling with the canonical labeling 
 * 	  of the vertices of Graph.
 * 	
 * 	- get_ptn(Partition)
 * 	  unify Partition with the partitioning
 * 	  of the vertices of Graph.
 * 	
 * 	- get_perm(Permutation)
 * 	  unify Permutation with the permutation
 * 	  of the vertices from Graph to CanonicGraph.
 * 	
 * 	- get_orbits(Orbits)
 * 	  unify Orbits with the orbits of the 
 * 	  vertices of graph Graph.
 * 	
 * 	- get_cg(Canonic)
 * 	  unify Canonic with the canonic graph.
 * 	
 * 	- coloring(Coloring)
 * 	  supply a vertex coloring for Graph.
 * **********************************************
 */
densenauty(N, G, Lab, Ptn, Perm, Orbits, Cg, Opts) :-
	(   var(N)
	->  option(infmt(InFmt), Opts),
	    detect_graph_size(InFmt, G, N)
	;   must_be(nonneg, N)
	),
	'$densenauty'(N, G, Lab, Ptn, Perm, Orbits, Cg, Opts).

densenauty(N, G, Opts) :-
	must_be(list, Opts),
	option(infmt(InFmt), Opts, adj_matrix),
	option(cgfmt(CgFmt), Opts, adj_matrix),
	option(digraph(Digraph), Opts, false),
	(   var(N)
	->  detect_graph_size(InFmt, G, N)
	;   must_be(nonneg, N)
	),
	(   option(get_lab(Lab), Opts)
	->  GetLab = true
	;   GetLab = false
	),
	(   option(get_ptn(Ptn), Opts)
	->  GetPtn = true
	;   GetPtn = false
	),
	(   option(get_perm(Perm), Opts)
	->  GetPerm = true
	;   GetPerm = false
	),
	(   option(get_orbits(Orb), Opts)
	->  GetOrb = true
	;   GetOrb = false
	),
	(   option(get_cg(Cg), Opts)
	->  GetCg = true 
	;   GetCg = false
	),
	DOpts = [ infmt(InFmt),
		  cgfmt(CgFmt),
		  get_lab(GetLab),
		  get_ptn(GetPtn),
		  get_perm(GetPerm),
		  get_orbits(GetOrb),
		  get_cg(GetCg),
		  digraph(Digraph)
		| Rest
		],
	(   option(coloring(Coloring), Opts)
	->  Rest = [coloring(Coloring)]
	;   Rest = []
	),
	densenauty(N, G, Lab, Ptn, Perm, Orb, Cg, DOpts).

/*
 * **********************************************
 * canonic_graph(+NVert, +Graph, ?Canonic)
 * 
 * unify Canonic with the canonic adjacency matrix
 * of Graph, which is also an adjacency matrix with
 * NVert vertices.
 * **********************************************
 */
canonic_graph(N, G, C) :-
	(   var(N)
	-> '$skip_list'(N, G, T),
	   T == []
	;  must_be(integer, N)
	),
	% we don't need the partitioning
	% only the canonic graph:
	Opts =  [ get_lab(false),
		  get_ptn(false),
		  get_perm(false),
		  get_orbits(false)
		],
	'$densenauty'(N,G,_,_,_,_,C,Opts).

canonic_graph(N, InFmt, OutFmt, G, C) :-
	detect_graph_size(InFmt, G, N),
	% we don't need the partitioning
	% only the canonic graph:
	Opts = 	[ infmt(InFmt), 
		  cgfmt(OutFmt),
		  get_lab(false),
		  get_ptn(false),
		  get_perm(false),
		  get_orbits(false),
		  get_cg(true)
		],
	'$densenauty'(N,G,_,_,_,_,C,Opts).

canonic_graph(N,InFmt,OutFmt,Graph,Perm,Canonic) :-
	detect_graph_size(InFmt, Graph, N),
	ignore(InFmt  = adj_matrix),
	ignore(OutFmt = adj_matrix),
	densenauty(N, Graph, [get_perm(Perm), get_cg(Canonic), infmt(InFmt), cgfmt(OutFmt)]).

/*
 * **********************************************
 * canonic_graphs(+NVerty, +Graphs, ?Canonic)
 * 
 * unify Canonic with the canonic representatives
 * of Graphs. bot Graphs and Canonic are adjacecy
 * matrices.
 * 
 * This prediacte is similar to the shortg utility
 * of nauty.
 * 
 * @see shortg in the nauty package
 * **********************************************
 */
canonic_graphs(N, Gs, Cs) :-
	maplist(canonic_graph(N), Gs, Cs0),
	% reduce duplicates
	sort(Cs0, Cs).

/*
 * **********************************************
 * 
 * **********************************************
 */
isomorphic_graphs(N, Graph1, Graph2, Perm, Canonic, Opts) :-
	'$isomorphic'(N, Graph1, Graph2, Perm, Canonic, Opts).

/*
 * **********************************************
 * 
 * **********************************************
 */
graph_convert(N, InFmt, OutFmt, In, Out) :-
	integer(N),
	N > 0, !,
	'$graph_convert'(N, InFmt, In, OutFmt, Out).
graph_convert(N, InFmt, OutFmt, In, Out) :-
	var(N),
	must_be(nonvar, InFmt),
	detect_graph_size(InFmt, In, N), !,
	graph_convert(N, InFmt, OutFmt, In, Out).
graph_convert(0, _InFmt, OutFmt, _In, Out) :-
	!,
	(   (   OutFmt == adj_matrix 
	    ;   OutFmt == adj_lists 
	    ;   OutFmt == edge_list 
	    ;   OutFmt == upper_triangle
	    ;   OutFmt == upper_triangle_flat
	    ;   OutFmt == graph6_chars
	    ;   OutFmt == graph6_codes
	    ;   OutFmt == directed_edge_list
	    ;   OutFmt == permutation_list
	    ;   OutFmt == permutation_pairs_list
	    ;   OutFmt == sat_adj_matrix
            )
	->  Out = []
	;   (   OutFmt == upper_triangle_char2
	    ;   OutFmt == upper_triangle_char16
	    ;   OutFmt == graph6_atom
	    ;   OutFmt == digraph6_atom
	    )
	->  Out = ''
	;   OutFmt == graph6_string
	->  Out = ""
	).
graph_convert(N, InFmt, OutFmt, _In, _Out) :-
	domain_error(graph_convert(N, InFmt, OutFmt), impossible_conversion).

detect_graph_size(_, _, N) :-
	nonvar(N), !, must_be(nonneg, N).
detect_graph_size(adj_matrix, Term, N) :-
	!, proper_length(Term, N).
detect_graph_size(upper_triangle, Term, N) :-
	!, proper_length(Term, N).
detect_graph_size(upper_triangle_flat, Term, N) :-
	!, proper_length(Term, M),
	N is ceil((1 + sqrt(1 + 4*M)) / 2).
detect_graph_size(upper_triangle_char2, Term, N) :-
	!, atom_length(Term, M),
	N is ceil((1 + sqrt(1 + 4*M)) / 2).
detect_graph_size(graph6_atom, Term, N) :-
	!, atom_length(Term, M),
	(   M >= 8
	->  sub_atom(Term, 0, 8, _, Sub),
	    atom_codes(Sub, Codes)
	;   atom_codes(Term, Codes)
	),
	detect_graph6_codes_size(Codes, N).
detect_graph_size(graph6_chars, Term, N) :-
	!, nonvar(Term),
	(   Term = [A1, A2, A3, A4, A5, A6, A7, A8|_]
	->  maplist(char_code, [A1, A2, A3, A4, A5, A6, A7, A8], Codes)
	;   Term = [A1, A2, A3, A4|_]
	->  maplist(char_code, [A1, A2, A3, A4], Codes)
	;   maplist(char_code, Term, Codes)
	),
	detect_graph6_codes_size(Codes, N).
detect_graph_size(graph6_codes, Term, N) :-
	!, nonvar(Term),
	detect_graph6_codes_size(Term, N).
detect_graph_size(graph6_string, Term, N) :-
	!, string_length(Term, M),
	(   M >= 8
	->  sub_string(Term, 0, 8, _, Sub),
	    string_codes(Sub, Codes)
	;   string_codes(Term, Codes)
	),
	detect_graph6_codes_size(Codes, N).
detect_graph_size(digraph6_atom, Term, N) :-
	!, atom_length(Term, M),
	(   M >= 9
	->  sub_atom(Term, 1, 8, _, Sub)
	;   sub_atom(Term, 1, _, 0, Sub)
	),
	atom_codes(Sub, Codes),
	detect_graph6_codes_size(Codes, N).
detect_graph_size(sat_adj_matrix, Term, N) :-
	!, proper_length(Term, N).

detect_graph6_codes_size([C1|_], N) :-
	C1 \== 0'~, !,
	N is C1 - 63.
detect_graph6_codes_size([0'~, C1, C2, C3|_], N) :-
	C1 \== 0'~, !,
	N is ((C1 - 63) << 12) +
	     ((C2 - 63) <<  6) +
	      (C3 - 63).
detect_graph6_codes_size([0'~, 0'~, C1, C2, C3, C4, C5, C6|_], N) :-
	C1 \== 0'~, !, 
	N is ((C1 - 63) << 30) + 
	     ((C2 - 63) << 24) + 
	     ((C3 - 63) << 18) + 
	     ((C4 - 63) << 12) + 
	     ((C5 - 63) <<  6) + 
	      (C6 - 63).

/*
 * **********************************************
 * graph_convert_file(?NVert, ?InFmt, ?OutFmt, +FileIn, +FileOut)
 * graph_convert_file(?NVert, ?InFmt, ?OutFmt, +FileIn, +FileOut, +Options)
 * 
 * FileIn is either a file which is opened in read
 * mode, or a stream with mode(read). FileOut is
 * either a file which is opened in write mode or
 * a stream with mode(write/append/update).
 * 
 * For each graph term read from FileIn a graph
 * term is written to FileOut.
 * **********************************************
 */
graph_convert_file(N, InFmt, OutFmt, FileIn, FileOut) :-
	graph_convert_file(N, InFmt, OutFmt, FileIn, FileOut, []).

graph_convert_file(N, InFmt, OutFmt, FileIn, FileOut, Options) :-
	option(write_mode(WriteMode), Options, write),
	setup_call_cleanup(
		stream_or_open(FileIn, read, In),
		setup_call_cleanup(
			stream_or_open(FileOut, WriteMode, Out),
			graph_convert_stream(N, InFmt, OutFmt, In, Out, Options),
			stream_or_close(FileOut, Out)
		),
		stream_or_close(FileIn, In)
	).

graph_convert_stream(N, InFmt, OutFmt, In, Out, Options) :-
	option(read_graph(Reader), Options, read_term),
	option(portray_graph(Portray), Options, portray_graph_clause),
	option(parse_graph_error(Error), Options, error),
	must_be(callable, Portray),
	must_be(oneof([error, warning, info, ignore]), Error),
	graph_convert_stream_(Reader, [N, InFmt, OutFmt], In, Out, Portray, Error, Options).

graph_convert_stream_(Reader, GraphFormat, In, Out, Portray, Error, Options) :-
	call(Reader, In, Term, Options),
	must_be(nonvar, Term),
	Term \== end_of_file, !,
	parse_graph_convert_stream_term(Term, GraphFormat, Out, Portray, Error, Options),
	graph_convert_stream_(Reader, GraphFormat, In, Out, Portray, Error, Options).
graph_convert_stream_(_, _, _, _, _, _, _).

parse_graph_convert_stream_term(Term, [N, InFmt, OutFmt], Out, Portray, _, Options) :-
	atomic(Term), !,
	graph_convert(N, InFmt, OutFmt, Term, Term0),
	call(Portray, Out, Term0, [graph_convert(N, InFmt, OutFmt, Term), read_term(Term)|Options]).
parse_graph_convert_stream_term(Term, [N, InFmt, OutFmt], Out, Portray, _, Options) :-
	compound(Term),
	Term = [_|_], !,
	graph_convert(N, InFmt, OutFmt, Term, Term0),
	call(Portray, Out, Term0, [graph_convert(N, InFmt, OutFmt, Term), read_term(Term)|Options]).
parse_graph_convert_stream_term(:- Header, [N, InFmt, OutFmt], _, _, _, _) :-
	compound(Header),
	compound_name_arity(Header, _, Arity),
	Arity > 1, !,
	(   Arity == 2
	->  arg(1, Header, InFmt0),
	    arg(2, Header, OutFmt0),
	    (   InFmt = InFmt0,
	        OutFmt = OutFmt0
	    ;   domain_error('in/out formats', [InFmt0, OutFmt0] \= [InFmt, OutFmt])
	    ), !
	;   arg(1, Header, N0),
	    arg(2, Header, InFmt0),
	    arg(3, Header, OutFmt0),
	    (   N = N0,
	        InFmt = InFmt0,
	        OutFmt = OutFmt0
	    ;   domain_error('nvert, in/out formats', [N0, InFmt0, OutFmt0] \= [N, InFmt, OutFmt])
	    ), !
	).
parse_graph_convert_stream_term(Term, [N, InFmt, OutFmt], Out, Portray, _, Options) :-
	compound(Term),
	compound_name_arity(Term, _, Arity),
	Arity > 0, !,
	(   Arity == 1
	->  arg(1, Term, Graph),
	    [N0, InFmt0, OutFmt0] = [N, InFmt, OutFmt]
	;   Arity == 2
	->  arg(1, Term, N0),
	    arg(2, Term, Graph),
	    [InFmt0, OutFmt0] = [InFmt, OutFmt]
	;   Arity == 3
	->  arg(1, Term, N0),
	    arg(2, Term, InFmt0),
	    arg(3, Term, Graph),
	    OutFmt0 = OutFmt
	;   arg(1, Term, N0),
	    arg(2, Term, InFmt0),
	    arg(3, Term, OutFmt0),
	    arg(4, Term, Graph)
	),
	graph_convert(N0, InFmt0, OutFmt0, Graph, Graph0),
	call(Portray, Out, Graph0, [graph_convert(N, InFmt, OutFmt, Graph), read_term(Term)|Options]).
parse_graph_convert_stream_term(Term, _, _, _, Error, _) :-
	(   Error == error
	->  densenauty_parse_error(error, graph_convert_term, Term)
	;   Error == warning
	->  print_message(warning, densenauty_parse_warning(warning, graph_convert_term, Term))
	;   Error == info
	->  print_message(informational, densenauty_parse_warning(info, graph_convert_term, Term))
	;   true
	).

portray_graph_clause(Out, Graph, _) :-
	portray_clause(Out, Graph).

format_graph_only(Out, Graph, Options) :-
	option(graph_format(Format), Options, '~w'),
	format(Out, Format, Graph),
	format(Out, '~n', []).

% TODO
% 	(   Arity == 2
% 	->  arg(1, Header, InFmt0),
% 	    arg(2, Header, OutFmt0),
% 	    ignore(InFmt = InFmt0),
% 	    ignore(OutFmt = OutFmt0)
% 	;   Arity == 3
% 	->  arg(1, Header, N0),
% 	    arg(2, Header, InFmt0),
% 	    arg(3, Header, OutFmt0),
% 	    ignore(N = N0),
% 	    ignore(InFmt = InFmt0),
% 	    ignore(OutFmt = OutFmt0)
% 	;   arg(1, Header, N0),
% 	    arg(2, Header, InFmt0),
% 	    arg(3, Header, OutFmt0),
% 	    arg(4, Header, Options0),
% 	    ignore(N = N0),
% 	    ignore(InFmt = InFmt0),
% 	    ignore(OutFmt = OutFmt0)
% 	),
% 	(   var(Options0)
% 	->  par

densenauty_parse_error(Severity, Expected, Got) :-
	throw(error(densenauty_parse_error(Severity, Expected, Got), _)).

prolog:message(densenauty_parse_error(Severity, Expected, Got)) -->
	["densenauty: parse ~w: expected `~w', got `~w'"-[Severity, Expected, Got]].

stream_or_open(File, Mode, Stream) :-
	is_stream(File), !,
	stream_property(File, mode(Mode0)),
	(   Mode == Mode0
	->  Stream = File
	;   domain_error(stream_mode(Mode), Mode0)
	).
stream_or_open(File, Mode, Stream) :-
	open(File, Mode, Stream).

stream_or_close(File, _) :-
	is_stream(File), !.
stream_or_close(_, Stream) :-
	close(Stream).

read_atomic_line(Stream, Atom, _Options) :-
    \+ at_end_of_stream(Stream),
    get_code(Stream, Code),
    read_atomic_line_codes_(Code, Stream, Codes),
    atom_codes(Atom, Codes).

read_term_line(Stream, Term, Options) :-
    option(term_bindings(Bindings), Options, _),
    read_atomic_line(Stream, Atom, Options),
    atom_to_term(Atom, Term, Bindings).

read_atomic_line_codes_(Last, Stream, Codes) :-
    (   code_type(Last, end_of_line)
    ->  Codes = []
    ;   code_type(Last, end_of_file)
    ->  Codes = []
    ;   Codes = [Last|Rest],
        get_code(Stream, Next),
        read_atomic_line_codes_(Next, Stream, Rest)
    ).

/*
 * **********************************************
 * group_graphs_by_canonic(+NVert, +Graphs, ?Grouped)
 * group_graphs_by_canonic(+NVert, +Graphs, ?Grouped, +Options)
 * 
 * Grouped is a pairs list with whose values are
 * the lists containing equivalence classes of Graphs 
 * under graph isomorphism, and whose keys are the
 * canonic form (under nauty) of those graphs.
 * 
 * options:
 * 	- infmt(Fmt)
 * 	  graphs input format
 * 	  
 * 	- cgfmt(Fmt)
 * 	  keys output format
 * **********************************************
 */
group_graphs_by_canonic(N, Graphs, Grouped) :-
	group_graphs_by_canonic(N, Graphs, Grouped, []).

group_graphs_by_canonic(N, Graphs, Grouped, Opts) :-
	option(infmt(InFmt), Opts, adj_matrix),
	option(cgfmt(CgFmt), Opts, graph6_atom),
	group_graphs_by_canonic_pairs(Graphs, Pairs, N, InFmt, CgFmt),
	keysort(Pairs, Sort),
	group_pairs_by_key(Sort, Grouped).

group_graphs_by_canonic_pairs([],[],_,_,_).
group_graphs_by_canonic_pairs([G|Gs], [H-G|Hs], N, In, Cg) :-
	canonic_graph(N, In, Cg, G, _, H),
	group_graphs_by_canonic_pairs(Gs, Hs, N, In, Cg).

%  Graphs = ['DRo','Dbg','DdW',
%               'DLo','D[S','DpS',
%               'DYc','DqK','DMg',
%               'DkK','Dhc','DUW'],
%     maplist(graph_convert(5, graph6_atom, adj_matrix), Graphs, AdjMatrices)

/*
 * **********************************************
 * non_isomorphic(+NVert, +Graphs, ?NonIsomorphic)
 * non_isomorphic(+NVert, +Graphs, ?NonIsomorphic, +Options)
 * 
 * NonIsomorphic contains all non-isomorphic graphs
 * from Graphs.
 * 
 * options:
 * 	- select(Callable)
 * 	  used to select the representative from
 * 	  each isomorphism equivalence class.
 * 	  it is called once(Callable(Member, Class))
 * **********************************************
 */
non_isomorphic(N, Graphs, NonIsomorphic) :-
	non_isomorphic(N, Graphs, NonIsomorphic, []).

non_isomorphic(_, [], [], _) :- !.
non_isomorphic(_, [G], [G], _) :- !.
non_isomorphic(N, [A,B], Cs, Opts) :- !,
	option(select(Select), Opts, min_member),
	(   isomorphic_graphs(N, A, B, _, _, Opts)
	->  Cs = [C],
	    call(Select, C, [A,B]), !
	;   Cs = [A, B]
	).
non_isomorphic(N, Graphs, NonIsomorphic, Opts) :-
	option(select(Select), Opts, min_member),
	group_graphs_by_canonic(N, Graphs, Grouped, Opts),
	to_non_isomorphic(Grouped, NonIsomorphic, Select).

to_non_isomorphic([], [], _).
to_non_isomorphic([_-Gs|Cs], [H|Hs], Select) :-
	call(Select, H, Gs), !,
	to_non_isomorphic(Cs, Hs, Select).
