/**
 * **********************************************
 * @file geng.pl
 * 
 * an implementation of gtools' geng using the
 * pl-gtools library.
 * **********************************************
 * in bash:
 * --------
 * 
 * $ for i in `seq 1 9` ; do 
 * 	pl-nauty/nauty26r3/geng -q $i | wc -l ; 
 * done
 * 
 * 1
 * 2
 * 4
 * 11
 * 34
 * 156
 * 1044
 * 12346
 * 274668
 *
 * in prolog:
 * ----------
 * 
 * ?- between(1,9,I), 
 *    findall(G, geng(I,G),Gs), 
 *    length(Gs,N), 
 *    writeln(geng(I) := N), fail.
 *
 *  geng(1) := 1
 *  geng(2) := 2
 *  geng(3) := 4
 *  geng(4) := 11
 *  geng(5) := 34
 *  geng(6) := 156
 *  geng(7) := 1044
 *  geng(8) := 12346
 *  geng(9) := 274668
 * **********************************************
 * @author Michael Frank
 * **********************************************
 */
:- module(geng, [geng/2]).

/*
 * fetch pl-gtools
 */
:- use_module(nauty('pl-gtools')).

/*
 * **********************************************
 * geng(+N, ?Graph)
 * 
 * unify Graph with non-isomorphic N vertices
 * graphs. upon backtracking - all graphs will be
 * generated (in graph6 format).
 * **********************************************
 */
geng(N, Graph) :-
	gtools_fork_exec(geng:parent_geng(Graph), geng:child_geng(N)).

parent_geng(Graph,Read) :-
	gtools_fetch(Read, Graph).

child_geng(N,Stream) :-
	gtools_exec('nauty26r3', geng, ['-q', N], _, Stream, _).
