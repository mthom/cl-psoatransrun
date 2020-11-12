/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Prolog server for PSOATransRun implementations, for Scryer Prolog.

   Written in October 2020 by Mark Thom (markjordanthom@gmail.com)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(psoatransrun_server, []).

:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

start_server :-
    socket_server_open('127.0.0.1':Port, ServerSocket),
    nl, write(Port), nl,
    socket_server_accept(ServerSocket, _, Stream, [eof_action(eof_code)]),
    eval_loop(Stream),
    close(Stream),
    socket_server_close(ServerSocket).


eval_loop(Stream) :-
    read_term(Stream, Term, [variable_names(VNNames)]),
    (  Term == end_of_file ->
       true
    ;
       read_term(Stream, _, [variable_names(UVNNames)]),
       split_vars(VNNames, UVNNames, RVNNames),
       catch(call(Term), _, false),
       phrase(compile_solution_string(RVNNames), VarString),
       (  RVNNames == [] ->
          write_term(Stream, 'Yes', [])
       ;
          write_term(Stream, VarString, [])
       ),
       write_term(Stream, '\n', []),
       flush_output(Stream),
       false
    ;
       write_term(Stream, 'No\n', []),
       flush_output(Stream),
       eval_loop(Stream)
    ).


split_vars([], _, []).
split_vars([VN | VNs], UVNNames, RVNs) :-
    (  member(VN, UVNNames) ->
       split_vars(VNs, UVNNames, RVNs)
    ;
       RVNs = [VN | RVNs1],
       split_vars(VNs, UVNNames, RVNs1)
    ).


compile_solution_string([]) --> [].
compile_solution_string([VN=Term|VNs]) -->
    {  write_term_to_chars(VN, [], VNChars),
       write_term_to_chars(Term, [quoted(true)], TermChars) },
    "'='(",
    VNChars,
    ",",
    TermChars,
    ")",
    (  { VNs \== [] } ->
       ", "
    ;
       { true }
    ),
    compile_solution_string(VNs).


:- initialization(start_server).
