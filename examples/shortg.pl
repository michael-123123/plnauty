/**
 * **********************************************
 * @file shortg.pl
 * 
 * an implementation of gtools' shortg using the 
 * pl-gtools module
 * **********************************************
 * in bash:
 * --------
 * 
 * $ ./geng -q 4 | ./shortg -q
 * 
 * C?
 * C@
 * CB
 * CF
 * CJ
 * CN
 * CR
 * C^
 * C`
 * Cr
 * C~
 *
 * in prolog:
 * ----------
 * 
 * ?- N=4, findall(G, geng(N,G), Gs),
 *    shortg(Gs, Hs), maplist(writeln, Hs).
 * 
 * C?
 * C@
 * CB
 * CF
 * CJ
 * CN
 * CR
 * C^
 * C`
 * Cr
 * C~
 * 
 * N = 4,
 * Gs = ['C?', 'CC', 'CE', 'CF', 'CQ', 'CU', 'CT', 'CV', 'C]'|...],
 * Hs = ['C?', 'C@', 'CB', 'CF', 'CJ', 'CN', 'CR', 'C^', 'C`'|...].
 * **********************************************
 * @author Michael Frank
 * **********************************************
 */
:- module(shortg, [shortg/2]).

/*
 * fetch pl-gtools module
 */
:- use_module(nauty('pl-gtools')).

/*
 * **********************************************
 * writeln/2
 * 
 * compatibility with swipl 6 and lower, that 
 * do not include writeln(Stream, Term).
 * **********************************************
 */
:- if((current_prolog_flag(version_data, swi(Maj,_,_,_)), Maj < 7)).

writeln(Stream, Writeln) :-
	write(Stream, Writeln),
	nl(Stream).

:- endif.

/*
 * **********************************************
 * shortg(+In, ?Out)
 * 
 * canonize a list of input graphs In and unify 
 * the result with Out list.
 * **********************************************
 */
shortg(In, Out) :-
	gtools_fork_exec_bidi(shortg:parent_shortg(In, Out), shortg:child_shortg).

%
% parent writes all graphs in In to pipe
% then closes write end, and waits for 
% child to finish calling shortg. 
% eventually parent reads all output 
% graphs from read end.
% 
parent_shortg(In, Out, PRead, PWrite) :-
	% write input graphs
	maplist(writeln(PWrite), In),
	flush_output(PWrite),
	close(PWrite),
	!,
	% read output graphs
	findall(O, gtools_fetch(PRead, O), Out),
	close(PRead),
	!.

child_shortg(CRead, CWrite) :-
	gtools_exec('nauty26r3', shortg, ['-q'], CRead, CWrite, _).
