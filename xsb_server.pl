/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Prolog server for PSOATransRun implementations, for XSB Prolog.

   Written in October 2020 by Mark Thom (markjordanthom@gmail.com)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(xsb_server, []).

:- use_module(file_io, [fd2ioport/2, file_close/1, file_flush/2, fmt_write_string/3]).
:- use_module(xsb_writ, [file_write_canonical/2]).
:- use_module(lists, [member/2]).
:- use_module(string, [term_to_codes/3]).

start_server :-
    eval_loop.

eval_loop :-
    read_term(user_input, Term, [variable_names(VNNames)]),
    (  Term == end_of_file ->
       true
    ;
       read_term(user_input, _, [variable_names(UVNNames)]),
       split_vars(VNNames, UVNNames, RVNNames),
       catch(call(Term), _, false),
       compile_solution_string(RVNNames, VarString),
       (  ( var(VarString) ; VarString == "" ) ->
          write_string("Yes")
       ;
          write_string(VarString)
       ),
       false
    ;
       write_string("No"),
       eval_loop
    ).

write_string(Codes) :-
    fmt_write_string(OutString, "%s\n", args(Codes)),
    write(OutString).


split_vars([], _, []).
split_vars([VN | VNs], UVNNames, RVNs) :-
    (  member(VN, UVNNames) ->
       split_vars(VNs, UVNNames, RVNs)
    ;
       RVNs = [VN | RVNs1],
       split_vars(VNs, UVNNames, RVNs1)
    ).

compile_solution_string(VNs, VarString) :-
    compile_solution_string_(VNs, "", VarString, 0).

compile_solution_string_([], VarString, VarString, _).
compile_solution_string_([VN=Value|VNs], PrefixString, VarString, VarCount0) :-
    (  var(Value) ->
       fmt_write_string(NamedVarString, "'Var%d'", args(VarCount0)),
       term_to_codes(VN=NamedVarString, [quoted(false), ignore_ops(true)], Codes),
       (  PrefixString == "" ->
          fmt_write_string(VarString0, "%s", args(Codes))
       ;
          fmt_write_string(VarString0, "%s, %s", args(PrefixString, Codes))
       ),
       Value = NamedVarString,
       VarCount is VarCount0 + 1
    ;
       replace_char_lists_with_strings(Value, Y),
       term_to_codes(VN=Y, [quoted(false), ignore_ops(true)], Codes),
       (  PrefixString == "" ->
          fmt_write_string(VarString0, "%s", args(Codes))
       ;
          fmt_write_string(VarString0, "%s, %s", args(PrefixString, Codes))
       ),
       VarCount = VarCount0
    ),
    compile_solution_string_(VNs, VarString0, VarString, VarCount).


maplist(_, [], []).
maplist(Pred, [X|Xs], [Y|Ys]) :-
    call(Pred, X, Y),
    maplist(Pred, Xs, Ys).

atom_quoted(Atom, NewAtom) :-
    atom_concat('\'', Atom, NewAtom0),
    atom_concat(NewAtom0, '\'', NewAtom).

replace_char_lists_with_strings(X, Y) :-
    (  number(X) ->
       X = Y
    ;  atom(X) ->
       (  ( atom_concat('_', _, X) ; atom_concat('<', _, X) ) ->
          fmt_write_string(Y, "'%s'", args(X))
       ;
          fmt_write_string(Y, "%s", args(X))
       )
    ;  is_charlist(X) ->
       fmt_write_string(Y, "\"%s\"", args(X))
    ;
       X =.. [F | Args],
       (  ( atom_concat('_', _, F) ; atom_concat('<', _, F) ) ->
          atom_quoted(F, NewF)
       ;
          NewF = F
       ),
       maplist(replace_char_lists_with_strings, Args, NewArgs),
       !,
       Y =.. [NewF | NewArgs]
    ).

:- initialization(start_server).
