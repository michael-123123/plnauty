/**
 * **********************************************
 * pl-gtools integration for gtools in prolog
 * 
 * a framework to execute nauty's gtools using 
 * unix pipes in prolog.
 * **********************************************
 */
:- module('pl-gtools',
	[ gtools_fork_exec/2,
	  gtools_fork_exec_bidi/2,
	  gtools_exec/6,
	  gtools_cmd/3,
	  gtools_fetch/2,
	  gtools_fetch/3
	]).

/*
 * **********************************************
 * gtools_fork_exec(+Parent, +Child)
 * 
 * gtools_fork_exec will create a pipe and then
 * fork. The child process will close the read end
 * of the pipe and do call(Child, Write) followed by 
 * a halt, where Write is the write end of the pipe.
 * the parent process will close the write end of
 * the pipe and call(Parent, Read) where Read is 
 * the read end of the pipe.
 * 
 * currently this predicate is not a meta predicate.
 * this might change in the future.
 * 
 * this is a general framework used to call
 * nauty applications that produce pure graph
 * output (e.g, one graph6 per line).
 * 
 * in most cases Child is a wrapper for gtools_exec/6
 * and parent is a wrapper for gtools_fetch/2.
 * 
 * @see examples/geng.pl for a geng implementation
 * **********************************************
 */
gtools_fork_exec(Parent, Child) :-
	pipe(Read, Write),
	fork(Pid),
	!,
	(Pid == child
	-> (close(Read),
	    call(Child, Write),
	    halt)
	;
	   (close(Write),
	    call(Parent, Read))).

/*
 * **********************************************
 * gtools_fork_exec_bidi(+Parent, +Child)
 * 
 * same as gtools_fork_exec except it opens a 
 * bidirectional channel between parent and child
 * in the form of two pipes.
 * 
 * Parent is called with two ends of a pipe:
 * one to read from and one to write to.
 * Child is called similarly with to ends of a pipe.
 * 
 * @see examples/shortg.pl for shortg implementation
 * **********************************************
 */
gtools_fork_exec_bidi(Parent, Child) :-
	pipe(PRead, CWrite),
	pipe(CRead, PWrite),
	fork(Pid),
	!,
	(Pid == child
	-> (close(PRead),
	    close(PWrite),
	    call(Child, CRead, CWrite),
	    halt)
	;
	   (close(CRead),
	    close(CWrite),
	    call(Parent, PRead, PWrite))).

/*
 * **********************************************
 * gtools_exec(+NautyBase, +CmdBase, +CmdArgs, 
 *             +InStream, +OutStream, +ErrStream)
 *
 * find and execute the nauty command with basename
 * CmdBase (see gtools_cmd/3 for more details on 
 * how the command is found). CmdArgs are added as 
 * command line arguments (should be [] for no 
 * arguments).
 * 
 * If present and is stream InStream replaces 
 * input stream, OutStream replaces output stream
 * and ErrStream replaces error stream. The 
 * replacement is done with dup/2.
 * **********************************************
 */
gtools_exec(NautyBase, CmdBase, CmdArgs, InStream, OutStream, ErrStream) :-
	gtools_cmd(NautyBase, CmdBase, Cmd),
	Exec =.. [Cmd|CmdArgs],
	dup_stream(InStream, 0),
	dup_stream(OutStream, 1),
	dup_stream(ErrStream, 2),
	exec(Exec),
	% should never be reached but just in case
	print_message(error, bad_exec),
	halt.

dup_stream(Stream, Dup) :-
	is_stream(Stream) ->
		(dup(Stream, Dup), close(Stream)) ; 
		true.

/*
 * **********************************************
 * gtools_cmd(+NautyBase, +CmdBase, ?Cmd)
 * 
 * search the absolute path of gtools cmd with 
 * basename CmdBase. NautyBase is the nauty build
 * directory.
 * 
 * this predicate assumes that NautyBase is 
 * accessible from the nauty search path (e.g,
 * that nauty(NautyBase/...) can be resolved).
 * **********************************************
 */
gtools_cmd(NautyBase, CmdBase, Cmd) :-
	file_search_path(nauty, PLNauty),
	absolute_file_name(PLNauty, Dirname),
	atomic_list_concat([Dirname, '/', NautyBase, '/', CmdBase], Cmd),
	exists_file(Cmd),
	!.

/*
 * **********************************************
 * gtools_fetch(+In, ?Next)
 * 
 * fetch the next result from running gtools
 * utility. Currently 'In' is the read end of the 
 * stream used to pipe the command through, but
 * theoretically it might be other things in the 
 * future.
 * **********************************************
 */
gtools_fetch(Stream, Read) :-
	is_stream(Stream),
	!,
	read_lines(Stream, Read).

gtools_fetch(Stream, Read, Opts) :-
	is_stream(Stream),
	!,
	read_lines(Stream, Read, Opts).

read_lines(Stream, Line) :-
	read_lines(Stream, Line, []).

read_lines(Stream, Line, Opts) :-
	(memberchk(blanks(true), Opts) -> NoBlanks = false ; NoBlanks = true),
	(memberchk(output(codes), Opts) -> Output = codes ; Output = atom),
	!,
	read_lines_opts(Stream, Line, NoBlanks, Output).
	

read_lines_opts(Stream, Line, NoBlanks, Output) :-
	repeat,
	read_line_codes(Stream, Codes, End),
	(End == -1 -> ! ; true),
	(NoBlanks, Codes == [] -> fail ; true),
	(Output == codes ->
		Line = Codes ; atom_codes(Line, Codes)).

read_line_codes(Stream, Line, Last) :-
	get_code(Stream, C),
	(end_of_line_code(C) ->
		Line = [], Last = C ;
		Line = [C|Cs], read_line_codes(Stream, Cs, Last)).

end_of_line_code(C) :-
	C == -1 ;
	code_type(C, end_of_line).
