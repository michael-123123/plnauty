:- module(auxs,
    [ addVertex/2,
      isRamsey/4,
      lex_star/2,
      no_clique/4,
      decode_matrix/2,
      adj_matrix_create/3,
      clique/5,
      random_adj_matrix/2
    ]).

/*
 * **********************************************
 * addVertex(+AdjMatrix, ?ExtendedAdjMatrix)
 * 
 * add a vertex to AdjMatrix in all possible ways
 * **********************************************
 */
addVertex(Matrix, [NewRow|NewRows]) :-
	NewRow = [0|Xs],
	addFirstCol(Matrix, Xs, NewRows).

addFirstCol([],[],[]).
addFirstCol([Row|Rows],[X|Xs],[[X|Row]|NewRows]) :-
	member(X,[0,1]),
	addFirstCol(Rows, Xs, NewRows).

/*
 * **********************************************
 * isRamsey(+S, +T, +N, +Graph)
 * 
 * test is Graph is a (S,T;N) Ramsey graph.
 * **********************************************
 */
isRamsey(S, T, N, Graph) :-
	forall( choose(N, S, Vs), \+ mono(0, Vs, Graph)),
	forall( choose(N, T, Vs), \+ mono(1, Vs, Graph)).

choose(N,K,C) :-
	numlist(1,N,Ns),
	length(C,K),
	choose(C,Ns).

choose([],[]).
choose([I|Is],[I|Js]) :-
	choose(Is,Js).
choose(Is,[_|Js]) :-
	choose(Is,Js).

mono(Color, Vs, Graph) :-
	cliqueEdges(Vs, Graph, Es),
	maplist(==(Color), Es).

cliqueEdges([],_,[]).
cliqueEdges([I|Is], Graph, Es) :-
	cliqueEdges(I, Is, Graph, Es0),
	cliqueEdges(Is, Graph, Es1),
	append(Es0, Es1, Es).

cliqueEdges(_,[],_,[]).
cliqueEdges(I, [J|Js], Graph, [E|Es]) :-
	nth1(I, Graph, Gi),
	nth1(J, Gi, E),
	cliqueEdges(I, Js, Graph, Es).

/*
 * **********************************************
 * clique(+NVert, +KSize, +AdjMat, ?Vertices, ?Edges)
 * 
 * on backtrack unify Vertices and Edges with all 
 * KSize cliques in a NVert x NVert adjacency 
 * matric AdjMat. Vertices is a sorted list of 
 * size KSize which indicates the clique's vertices,
 * and Edges is a list of size {KSize choose 2} 
 * containing all edges of the clique taken from
 * AdjMat.
 * **********************************************
 */
clique(NVert,KSize,AdjMat,Verts, Edges) :-
	choose(NVert,KSize,Verts),
	cliqueEdges(Verts, AdjMat, Clique),
	sort(Clique, Edges).

/*
 * **********************************************
 * 
 * **********************************************
 */
lex_star(M,Cs1-Cs2) :-
        length(M,N),
        findall(p(I,J),(between(1,N,I),between(I,N,J),I<J,J-I=\=2),List), 
        lex_star(List,M,Cs1-Cs2).

lex_star([],_,Cs-Cs).
lex_star([p(I,J)|List],M,Cs1-Cs3) :-
        nth1(I,M,Row1),
        nth1(J,M,Row2),
        shorten(I,J,Row1,Vec1),
        shorten(I,J,Row2,Vec2),
        Cs1=[bool_arrays_lex(Vec1,Vec2)|Cs2],
        lex_star(List,M,Cs2-Cs3).

shorten(I,J,Row,Vec) :-
        nth1(J,Row,_,Tmp),
        nth1(I,Tmp,_,Vec).

/*
 * **********************************************
 * 
 * **********************************************
 */
no_clique(Color, Size, Matrix, Cs1-Cs2) :-
	length(Matrix, NVert),
	findall(Clique, clique(NVert, Size, Matrix, Clique, _), Cliques),
	not_monochromatic(Cliques, Color, Matrix, Cs1-Cs2).

not_monochromatic([],_,_,Cs-Cs).
not_monochromatic([Clique|Cs],Color,Matrix,Cs1-Cs3) :-
	cliqueEdges(Clique, Matrix, Edges0),
	(Color == 0 
		-> Edges1 = Edges0
		;  negate(Edges0, Edges1)),
	Cs1 = [bool_array_or(Edges1)|Cs2],
	not_monochromatic(Cs,Color,Matrix,Cs2-Cs3).

negate([],[]).
negate([A|As],[-A|Bs]) :-
	negate(As,Bs).

/*
 * **********************************************
 * 
 * **********************************************
 */
decode_matrix(Matrix,Decode) :-
	maplist(maplist(decode_lit), Matrix, Decode).

decode_lit(Lit, Val) :-
	ground(Lit), !, Val is Lit.
decode_lit(Lit, Lit).

/*
 * **********************************************
 * 
 * **********************************************
 */
adj_matrix_create(NVert, Matrix, Diag) :-
	matrix_create(NVert, Matrix),
	transpose(Matrix, Matrix),
	diagonal(Matrix, Diag).

matrix_create(N, Matrix) :-
	length(Matrix, N),
	length_rows(Matrix, N).

length_rows([],_).
length_rows([R|Rs], N) :-
	length(R,N),
	length_rows(Rs, N).

transpose(E,[]) :-
	maplist(=([]), E).
transpose(M,[C|Cs]) :-
	column(M,C,R),
	transpose(R,Cs).

column([],[],[]).
column([[C|R]|Ms],[C|Cs],[R|Rs]) :-
	column(Ms, Cs, Rs).

% diagonal(A,B) :-
% 	writeln(diag(A,B)),fail.
diagonal([],_).
diagonal([[D|_]|Ms],D) :-
	column(Ms,_,Rs),
	diagonal(Rs, D).

/*
 * **********************************************
 * 
 * **********************************************
 */
random_adj_matrix(N, Adj) :-
	adj_matrix_create(N, Adj, 0),
	term_variables(Adj,Vars),
	maplist(random(0, 2), Vars).
