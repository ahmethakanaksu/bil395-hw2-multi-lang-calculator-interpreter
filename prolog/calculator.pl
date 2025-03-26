:- use_module(library(readutil)).
:- dynamic var/2.

% Entry point for the REPL
start :-
    write('Prolog Calculator Interpreter'), nl,
    write('Type "exit." to quit.'), nl,
    repl.

% Read-Eval-Print Loop
repl :-
    write('>> '),
    flush_output,             % ensure prompt is printed
    read_line_to_string(user_input, InputString),
    nl,                       % force newline after input to sync buffer
    ( InputString = "exit" ->
        write('Goodbye!'), nl
    ; ( term_string(Input, InputString) ->
          ( catch(handle_input(Input), Error,
                  (print_message(error, Error)))
          )
      ; write('Parse Error: Invalid syntax.'), nl ),
      repl
    ).

% Handle variable assignment or expression evaluation
handle_input(Var = Expr) :-
    eval(Expr, Value),
    ( number(Value) ->
        retractall(var(Var, _)),
        assertz(var(Var, Value)),
        format('~w = ~w~n', [Var, Value])
    ;
        write('Error: Invalid assignment.'), nl
    ).

handle_input(Expr) :-
    eval(Expr, Result),
    ( number(Result) ->
        format('Result: ~w~n', [Result])
    ;
        write('Error: Invalid expression.'), nl
    ).

% Expression evaluation rules
eval(X + Y, R) :- eval(X, RX), eval(Y, RY), R is RX + RY.
eval(X - Y, R) :- eval(X, RX), eval(Y, RY), R is RX - RY.
eval(X * Y, R) :- eval(X, RX), eval(Y, RY), R is RX * RY.
eval(X / Y, R) :-
    eval(X, RX), eval(Y, RY),
    ( RY =:= 0 ->
        write('Error: Division by zero!'), nl, fail
    ; R is RX / RY
    ).
eval(X ^ Y, R) :- eval(X, RX), eval(Y, RY), R is RX ** RY.
eval(Var, R) :-
    atom(Var),
    ( var(Var, R1) -> R = R1
    ; write('Error: Unknown variable: '), write(Var), nl, fail
    ).
eval(Number, Number) :-
    number(Number).
