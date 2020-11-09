/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The Prolog server for PSOATransRun implementations, for XSB Prolog.

   Written in October 2020 by Mark Thom (markjordanthom@gmail.com)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(xsb_server, []).

:- use_module(file_io, [fd2ioport/2, file_flush/2, fmt_write_string/3]).
:- use_module(xsb_writ, [file_write_canonical/2]).
:- use_module(lists, [member/2]).
:- use_module(socket, [
                  socket/2,
                  socket_accept/3,
                  socket_bind/3,
                  socket_connect/4,
                  socket_close/2,
                  socket_listen/3]).

:- use_module(string, [term_to_codes/3]).

xsb_port(6020). % XSB, in its infinite wisdom, requires ports to be instantiated in its socket API.

start_server :-
    socket(Socket, 0), % socket_server_open('127.0.0.1':Port, ServerSocket),
    xsb_port(Port),
    nl, write(Port), nl,
    socket_bind(Socket, Port, _), % socket_server_accept(ServerSocket, _, Stream, [eof_action(eof_code)]),
    socket_listen(Socket, 2, _), % listen for two pending connections.
    % the read stream.
    socket_accept(Socket, InSocket, _),
    fd2ioport(InSocket, InStream),
    % the write stream.
    socket_accept(Socket, OutSocket, _),
    fd2ioport(OutSocket, OutStream),
    eval_loop(InStream, OutStream),
    socket_close(Socket, _). % socket_server_close(ServerSocket).

eval_loop(InStream, OutStream) :-
    read_term(InStream, Term, [variable_names(VNNames)]),
    (  Term == end_of_file ->
       true
    ;
       read_term(InStream, _, [variable_names(UVNNames)]),
       split_vars(VNNames, UVNNames, RVNNames),
       catch(call(Term), _, false),
       compile_solution_string(RVNNames, VarString),
       (  VarString == "" ->
          file_write_string(OutStream, "Yes")
       ;
          file_write_string(OutStream, VarString)
       ),
       file_flush(OutStream, _),
       false
    ;
       file_write_string(OutStream, "No"),
       file_flush(OutStream, _),
       eval_loop(InStream, OutStream)
    ).

file_write_string(OutStream, Codes) :-
    fmt_write_string(OutString, "%s\n", args(Codes)),
    file_write(OutStream, OutString).


split_vars([], _, []).
split_vars([VN | VNs], UVNNames, RVNs) :-
    (  member(VN, UVNNames) ->
       split_vars(VNs, UVNNames, RVNs)
    ;
       RVNs = [VN | RVNs1],
       split_vars(VNs, UVNNames, RVNs1)
    ).

compile_solution_string(VNs, VarString) :-
    compile_solution_string_(VNs, "", VarString).

compile_solution_string_([], VarString, VarString).
compile_solution_string_([VN|VNs], PrefixString, VarString) :-
    replace_char_lists_with_strings(VN, RVN),
    term_to_codes(RVN, [quoted(false), ignore_ops(true)], Codes),
    (  PrefixString == "" ->
       fmt_write_string(VarString0, "%s", args(Codes))
    ;
       fmt_write_string(VarString0, "%s, %s", args(PrefixString, Codes))
    ),
    compile_solution_string_(VNs, VarString0, VarString).


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
       (  atom_concat('_', _, X) ->
          fmt_write_string(Y, "'%s'", args(X))
       ;
          fmt_write_string(Y, "%s", args(X))
       )
    ;  var(X) ->
       Y = X
    ;  is_charlist(X) ->
       fmt_write_string(Y, "\"%s\"", args(X))
    ;
       X =.. [F | Args],
       (  atom_concat('_',_, F) ->
          atom_quoted(F, NewF)
       ;
          NewF = F
       ),
       maplist(replace_char_lists_with_strings, Args, NewArgs),
       Y =.. [NewF | NewArgs]
    ).

:- initialization(start_server).

