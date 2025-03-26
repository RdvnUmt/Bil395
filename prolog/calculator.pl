:- initialization(start).

start :-
    write('Çıkmak için \'exit\' yazın'), nl,
    main_loop.

main_loop :-
    write('-> '),
    flush_output,
    read_line_to_string(user_input, Input),
    process_input(Input).

process_input("exit") :- !.
process_input(Input) :-
    tokenize(Input, Tokens),
    catch(
        (parse_expr(Tokens, Result, []) ->
            format('= ~w~n', [Result])
        ;   write('Hata: İfade anlaşılamadı'), nl),
        error(zero_division, Msg),
        (write('Hata: '), write(Msg), nl)
    ),
    main_loop.

tokenize(Input, Tokens) :-
    string_chars(Input, Chars),
    remove_spaces(Chars, NoSpaceChars),
    tokenize_chars(NoSpaceChars, Tokens).

remove_spaces([], []).
remove_spaces([' '|T], Result) :- remove_spaces(T, Result).
remove_spaces([H|T], [H|Result]) :- H \= ' ', remove_spaces(T, Result).

tokenize_chars([], []).
tokenize_chars([H|T], [Token|Tokens]) :-
    read_token(H, T, Token, Rest),
    tokenize_chars(Rest, Tokens).

read_token(C, T, Token, Rest) :-
    (is_digit(C) ->
        read_number([C|T], Token, Rest)
    ; is_operator(C) ->
        Token = C,
        Rest = T
    ; C = '(' ->
        Token = '(',
        Rest = T
    ; C = ')' ->
        Token = ')',
        Rest = T
    ;
        Token = C,
        Rest = T
    ).

read_number(Chars, Number, Rest) :-
    read_digits(Chars, Digits, Rest),
    number_chars(Number, Digits).

read_digits([], [], []).
read_digits([H|T], [H|Digits], Rest) :-
    is_digit(H), !,
    read_digits(T, Digits, Rest).
read_digits([H|T], [], [H|T]).

is_digit(C) :- char_type(C, digit).
is_operator(C) :- member(C, ['+', '-', '*', '/']).

parse_expr(Tokens, Result, Rest) :-
    parse_term(Tokens, Term, Rest1),
    (
        Rest1 = [Op|Rest2],
        is_addop(Op),
        parse_expr(Rest2, Term2, Rest), !,
        apply_op(Op, Term, Term2, Result)
    ;
        Result = Term,
        Rest = Rest1
    ).

parse_term(Tokens, Result, Rest) :-
    parse_factor(Tokens, Factor, Rest1),
    (
        Rest1 = [Op|Rest2],
        is_mulop(Op),
        parse_term(Rest2, Term, Rest), !,
        apply_op(Op, Factor, Term, Result)
    ;
        Result = Factor,
        Rest = Rest1
    ).

parse_factor(Tokens, Result, Rest) :-
    (
        Tokens = ['('|Rest1],
        parse_expr(Rest1, Result, [')'|Rest]), !
    ;
        Tokens = [Token|Rest],
        number(Token),
        Result = Token
    ).

is_addop(Op) :- member(Op, ['+', '-']).
is_mulop(Op) :- member(Op, ['*', '/']).

apply_op('+', A, B, Result) :- Result is A + B.
apply_op('-', A, B, Result) :- Result is A - B.
apply_op('*', A, B, Result) :- Result is A * B.
apply_op('/', A, B, Result) :-
    (B =:= 0 ->
        throw(error(zero_division, 'Sıfıra bölme hatası'))
    ;
        Result is A // B
    ). 