:- module(graph_formats, [swap_format/5, all_formats/5]).

:- use_module(nauty(examples/auxs)).
:- use_module(nauty(densenauty)).

% ?- auxs:random_adj_matrix(5, A), member(OutFmt, [adj_lists, edge_list, graph6_atom]), swap_format(5, adj_matrix, OutFmt, A,B).
swap_format(N, InFmt, OutFmt, In, Out) :-
	graph_convert(N, InFmt, OutFmt, In, Out),
	graph_convert(N, OutFmt, InFmt, Out, In0),
% 	writeln(in0 = In0),
	once(In == In0; throw(swap_format(N, InFmt, OutFmt, In, Out, In0))).

% ?- auxs:random_adj_matrix(5, A), all_formats(5,A,B,C,D).
% adjmat:=[[0,1,1,0,1],[1,0,1,1,0],[1,1,0,1,1],[0,1,1,0,1],[1,0,1,1,0]]
% adj_lists:=[1-[2,3,5],2-[3,4],3-[4,5],4-[5],5-[]]
% edge_list:=[1-2,1-3,1-5,2-3,2-4,3-4,3-5,4-5]
% graph6:=Dzk
% A = [[0, 1, 1, 0, 1], [1, 0, 1, 1, 0], [1, 1, 0, 1, 1], [0, 1, 1, 0, 1], [1, 0, 1, 1|...]],
% B = [1-[2, 3, 5], 2-[3, 4], 3-[4, 5], 4-[5], 5-[]],
% C = [1-2, 1-3, 1-5, 2-3, 2-4, 3-4, 3-5, 4-5],
% D = 'Dzk' ;
% false.
all_formats(N, AdjMatrix, AdjList, EdgeList, Graph6Atom) :-
	writeln(adjmat := AdjMatrix),
	graph_convert(N, adj_matrix, adj_lists, AdjMatrix, AdjList),
	writeln(adj_lists := AdjList),
	graph_convert(N, adj_lists, edge_list, AdjList, EdgeList),
	writeln(edge_list := EdgeList),
	graph_convert(N, edge_list, graph6_atom, EdgeList, Graph6Atom),
	graph_convert(N, graph6_atom, adj_matrix, Graph6Atom, AdjMatrix0),
	writeln(graph6 := Graph6Atom),
	(AdjMatrix \== AdjMatrix0 -> throw(all_format_diff(AdjMatrix,AdjMatrix0)) ; true).
