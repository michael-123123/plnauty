:- module(matrix,
	[ mat_length/2,
	  mat_transpose/2,
	  mat_diagonal_value/2,
	  adjmat/2,
	  random_adjmat/2
	]).



mat_transpose(A, []) :-
	maplist(=([]), A), !.
mat_transpose(A, [C|Cs]) :-
	maplist(nth1(1), A, C, X),
	mat_transpose(X, Cs).

mat_diagonal_value([], _).
mat_diagonal_value([[X|_]|R], X) :-
	maplist(nth1(1), R, _, R0),
	mat_diagonal_value(R0, X).

mat_length(A, B) :-
	length(A, B),
	lengths(A, B).

lengths([], _).
lengths([A|As], B) :-
	length(A, B),
	lengths(As, B).

adjmat(A, B) :-
	mat_length(A, B),
	mat_transpose(A, A),
	mat_diagonal_value(A, -1).


random_adjmat(A, B) :-
	adjmat(A, B),
	term_variables(A, Vs),
	maplist(random_oneof([-1, 1]), Vs).

random_oneof(A, B) :- random_member(B, A).

