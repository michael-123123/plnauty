/**
 * **********************************************
 * @file ramseyCG.pl
 * 
 * a ramsey 2-coloring graph iterator, using a 
 * "constrain & generate" approach; pruning with 
 * pl-nauty.
 * 
 * 
 * % constraint and generate - top level
 * genRamsey(S, T, N, Graphs) :-
 *     encode(ramsey(S,T,N), Matrix, Constraints),
 *     beeCompile(Constraints,CNF),
 *     projectVariables(Matrix, Booleans),
 *     solveAll(CNF,Booleans,Assignments),
 *     decode(Assignments,Matrix,Graphs0),
 *     maplist(canonic_graph(N), Graphs0, Graphs1),
 *     sort(Graphs1, Graphs).
 * 
 * @author Michael Frank
 * **********************************************
 */
:- module(ramseyCG,
	[ genRamseyCG/4,
	  genAllRamseyCG/4,
	  encodeRamseyCG/3,
	  encodeAllRamseyCG/4
	]).

/*
 * fetch libraries
 */
:- use_module(nauty(densenauty)).
:- use_module(nauty(examples/auxs)).

/*
 *
 */
:- if(user:file_search_path(bee,_)).

:- use_module(bee(auxRunExprAll),[runExprAll/5]).
:- use_module(bee(auxRunExpr),[runExpr/5]).
:- use_module(bee(bCompiler)).

:- endif.

/*
 * **********************************************
 * genRamseyCG(+S, +T, +N, ?Graphs)
 * 
 * encode and generate a single ramsey (S,T;N) graph
 * **********************************************
 */
genRamseyCG(S, T, N, Graph) :-
	solve(ramsey(S,T,N), Graph).

solve(Instance, Graph) :-
	nonvar(Instance),
	runExpr(
	    Instance,
	    Solution,
	    ramseyCG:encodeRamseyCG,
	    ramseyCG:decode,
	    ramseyCG:verify
	),
	Graph = Solution.

/*
 * **********************************************
 * genAllRamseyCG(+S, +T, +N, +Graphs)
 * 
 * encode and generate all non-isomorphic ramsey
 * (S,T;N) graphs.
 * **********************************************
 */
genAllRamseyCG(S, T, N, Graphs) :-
	solveAll(ramsey(S, T, N), Graphs).

solveAll(Instance, Graphs) :-
	nonvar(Instance),
	runExprAll(
	    Instance,
	    GraphsTmp0,
	    ramseyCG:encodeAllRamseyCG,
	    ramseyCG:decode,
	    ramseyCG:verify),
	Instance = ramsey(_,_,N),
	maplist(canonic_graph(N), GraphsTmp0, GraphsTmp1),
	sort(GraphsTmp1, Graphs).

/*
 * **********************************************
 * encodeRamseyCG(+Instance, +Map, ?Constraints)
 * 
 * encode an instance of ramsey graphs.
 * **********************************************
 */
encodeRamseyCG(Instance, Map, Constraints) :-
	encode(Instance, Map, Constraints-Tail),
	Tail = [].

encode(Instance, Map, Cs1-Cs4) :-
	must_be(nonvar, Instance),
	Instance = ramsey(S,T,N),
	must_be(integer, S),
	must_be(integer, T),
	must_be(integer, N),
	must_be(between(1,N), S),
	must_be(between(1,N), T),
	% 
	adj_matrix_create(N, Matrix, -1),
	Map = map(Matrix),
	lex_star(Matrix, Cs1-Cs2),
	%
	no_clique(0, S, Matrix, Cs2-Cs3),
	no_clique(1, T, Matrix, Cs3-Cs4).

/*
 * **********************************************
 * encodeAllRamseyCG(+Instance, +Map, +ProjectedVars, +Constraints)
 * 
 * encode an instance for the ramsey all solutions 
 * instance.
 * **********************************************
 */
encodeAllRamseyCG(Instance, Map, Vars, Constraints) :-
	encodeAll(Instance, Map, Vars, Constraints-Tail),
	Tail = [].

encodeAll(Instance, Map, (Bools, []), Constraints) :-
	encode(Instance, Map, Constraints),
	term_variables(Map, Bools).

/*
 * **********************************************
 * 
 * **********************************************
 */
decode(map(Matrix), Graph) :-
	decode_matrix(Matrix,Graph).

/*
 * **********************************************
 * 
 * **********************************************
 */
verify(ramsey(S,T,N),Graph) :-
	isRamsey(S,T,N,Graph).
