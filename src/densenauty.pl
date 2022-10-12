/**
 * **********************************************
 * 
 * **********************************************
 */
:- module(densenauty,
	[ densenauty/3,
	  densenauty/8,
	  canonic_graph/3,
	  canonic_graph/6,
	  canonic_graphs/3,
	  isomorphic_graphs/6,
	  graph_convert/5,
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

/*
 * **********************************************
 * 
 * **********************************************
 */
:- load_foreign_library('pl-nauty.so',install).
:- initialization('$densenauty_initialize').

/*
 * **********************************************
 * 
 * **********************************************
 */
densenauty(N, G, Lab, Ptn, Perm, Orbits, Cg, Opts) :-
	(var(N) ->
		('$skip_list'(N, G, T), T == []) ;
		must_be(nonneg, N)),
	'$densenauty'(N, G, Lab, Ptn, Perm, Orbits, Cg, Opts).

densenauty(N, G, Opts) :-
	(var(N) ->
		('$skip_list'(N, G, T), T == []) ;
		must_be(nonneg, N)),
	must_be(list, Opts),
	once( memberchk(infmt(InFmt), Opts) ; InFmt = adj_matrix ),
	once( memberchk(cgfmt(CgFmt), Opts) ; CgFmt = adj_matrix ),
	once( memberchk(digraph(Digraph), Opts) ; Digraph = false ),
	(memberchk(get_lab(Lab), Opts) ->
		GetLab = true ; GetLab = false),
	(memberchk(get_ptn(Ptn), Opts) ->
		GetPtn = true ; GetPtn = false),
	(memberchk(get_perm(Perm), Opts) ->
		GetPerm = true ; GetPerm = false),
	(memberchk(get_orbits(Orb), Opts) ->
		GetOrb = true ; GetOrb = false),
	(memberchk(get_cg(Cg), Opts) ->
		GetCg = true ; GetCg = false),
	DOpts = [infmt(InFmt),
		 cgfmt(CgFmt),
		 get_lab(GetLab),
		 get_ptn(GetPtn),
		 get_perm(GetPerm),
		 get_orbits(GetOrb),
		 get_cg(GetCg) | Rest],
	(memberchk(coloring(Coloring), Opts) ->
		Rest = [coloring(Coloring)] ; Rest = []),
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
	(var(N) ->
		('$skip_list'(N, G, T), T == []) ;
		must_be(integer, N)),
	% we don't need the partitioning
	% only the canonic graph:
	Opts = [get_lab(false),
		get_ptn(false),
		get_perm(false),
		get_orbits(false)],
	'$densenauty'(N,G,_,_,_,_,C,Opts).

canonic_graph(N,InFmt,OutFmt,Graph,Perm,Canonic) :-
	(var(N) ->
		('$skip_list'(N, Graph, T), T == []) ;
		must_be(nonneg, N)),
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
	'$graph_convert'(N, InFmt, In, OutFmt, Out).

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
group_graphs_by_canonic_pairs([G|Gs], [H|Hs], N, In, Cg) :-
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
