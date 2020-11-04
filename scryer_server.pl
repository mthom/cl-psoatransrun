/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Prolog server for PSOATransRun implementations, for Scryer Prolog.

   Written in October 2020 by Mark Thom (markjordanthom@gmail.com)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(psoatransrun_server, []).

:- use_module(library(atts)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

:- attribute ruleml_var/1.

start_server :-
    socket_server_open('127.0.0.1':Port, ServerSocket),
    nl, write(Port), nl,
    socket_server_accept(ServerSocket, _, Stream, [eof_action(eof_code)]),
    eval_loop(Stream),
    socket_server_close(ServerSocket).


eval_loop(Stream) :-
    read_term(Stream, Term, [variable_names(VNNames)]),
    (  Term == end_of_file ->
       true
    ;
       read_term(Stream, _, [variable_names(UVNNames)]),
       split_vars(VNNames, UVNNames, RVNNames),
       catch(findall(RVNNames, Term, Solutions0), _, false),
       sort(Solutions0, Solutions),
       (  Solutions == [[]] ->
          write_term(Stream, 'Yes\n', [])
       ;
          member(RVNNames1, Solutions),
          compile_solution_string(RVNNames1, VarString),
          write_term(Stream, VarString, []),
          write_term(Stream, '\n', []),
          false
       ),
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


compile_variable_names([], [], []).
compile_variable_names([VNName=Var | VNEqs], [NewVNName | VNNames], [Var | Vars]) :-
    write_term_to_chars(VNName, [], VNNameWithQ),
    VNNameWithQ = [_ | VNNamePostQ],
    NewVNName = ['?' | VNNamePostQ],
    compile_variable_names(VNEqs, VNNames, Vars).

compile_solution_string(VNEqs, VarString) :-
    compile_variable_names(VNEqs, VNNames, Vars),
    variable_preprocessing(Vars, 0),
    !,
    phrase(write_var_eqs(VNNames, Vars), VarString).

variable_preprocessing([], _).
variable_preprocessing([Term | Terms], VarCount0) :-
    (  var(Term) ->
       put_atts(Term, +ruleml_var(VarCount0)),
       VarCount1 is VarCount0 + 1,
       variable_preprocessing(Terms, VarCount1)
    ;
       variable_preprocessing(Terms, VarCount0)
    ).

phrase_maplist([Arg | Args], DCG, Delimiter) -->
    { DCGCall =.. [DCG, Arg] },
    phrase(DCGCall),
    (  { Args = [] } ->
       []
    ;
       Delimiter,
       phrase_maplist(Args, DCG, Delimiter)
    ).

write_psoa_term(Term) -->
    (  { var(Term) } ->
       { get_atts(Term, +ruleml_var(VarCount)) },
       format_("Var~d", [VarCount])
    ;  { partial_string(Term) } ->
       format_("\"~s\"", [Term])
    ;
       { functor(Term, F, _) },
       format_("~w", [F]),
       { Term =.. [_ | Args] },
       (  { Args = [_|_] } ->
          "(",
          phrase_maplist(Args, write_psoa_term, " "),
          ")"
       ;
          { true }
       )
    ).

write_var_eqs([], []) --> [].
write_var_eqs([VNName | VNNames], [Term | Terms]) -->
    { phrase(write_psoa_term(Term), TermString) },
    format_("~s=~s ", [VNName, TermString]),
    !,
    write_var_eqs(VNNames, Terms).


:- initialization(start_server).
