/**
 * **********************************************
 * @file ramseyGT.pl
 * 
 * a ramsey 2-coloring graph iterator, using a 
 * "generate & test" approach; pruning with pl-nauty
 * 
 * @author Michael Frank
 * **********************************************
 */
:- module(ramseyGT, [genRamseyGT/4]).

/*
 * fetch libraries
 */
:- use_module(nauty(densenauty)).
:- use_module(nauty(examples/auxs)).

/*
 * **********************************************
 * genRamseyGT(+S, +T, +N, ?Graphs)
 * 
 * incrementally generate all non-isomorphic r(S,T;N) 
 * graphs. 
 * **********************************************
 */
genRamseyGT(S, T, N, Graphs) :-
	genRamseyGT(0, S, T, N, [[]], Graphs).

genRamseyGT(I, S, T, N, Acc, Graphs) :-
	I =< N, !, I1 is I+1,
	extendRamsey(S, T, I, Acc, NewAcc),
% 	writeln(extend(S,T,I,Acc,NewAcc)),
	genRamseyGT(I1, S, T, N, NewAcc, Graphs).
genRamseyGT(_, _, _, _, Graphs, Graphs).

/*
 * **********************************************
 * extend a set of (non-isomorphic) r(S,T,N) graphs to their 
 * canonical r(S,T,N+1) extensions
 * **********************************************
 */
extendRamsey(S, T, N, Graphs, NewGraphs) :-
	N1 is N+1,
	findall(Canonic,
		(member(Graph, Graphs),
		 addVertex(Graph, NewGraph),
		 isRamsey(S, T, N1, NewGraph),
		 canonic_graph(N1, NewGraph, Canonic) % #(call to plnauty's interface)
		),
		GraphsTmp),
	sort(GraphsTmp, NewGraphs).
