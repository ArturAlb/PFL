% FILEPATH: /path/to/file.pl
:- use_module(library(lists)).

% Define the initial state of the game
initial_state([
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
], [8, 8, 1, 30]).

/*draw a board of 7x7 with lines and columns*/

draw_board([H|T]) :-
    write('  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('  -----------------------------\n'),
    draw_board([H|T], 1).

draw_board(_, 8) :- !.

draw_board([H|T], N) :-
    write(N),
    write(' | '),
    draw_line(H),
    nl,
    write('  -----------------------------\n'),
    N1 is N + 1,
    draw_board(T, N1).

draw_line([]) :- !.

draw_line([H|T]) :-
    write(H),
    write(' | '),
    draw_line(T).

/*place a piece in the board*/

place_piece(Board, Column, Line, NewBoard, 0) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'X', NewLineList),
    replace(Board, Line, NewLineList, NewBoard).

place_piece(Board, Column, Line, NewBoard, 1) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'Y', NewLineList),
    replace(Board, Line, NewLineList, NewBoard).

replace([_|T], 1, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

/*remove one piece from the board*/

remove_piece(Board, Column, Line, NewBoard) :-
    place_piece(Board, ' ', Column, Line, NewBoard).

/*check if the game is over*/

is_even(X) :-
    0 is X mod 2.

start_game(0).

start_game(60):-
    initial_state(B,Y),
    draw_board(B),
    write('Column: '),
    read(C),
    write('Line: '),
    read(L),
    place_piece(B, C, L, NB, 0),
    NN is 60-1,
    start_game(NN,NB).

start_game(N,B):- N>0,
    draw_board(B),
    write('Column: '),
    read(C),
    write('Line: '),
    read(L),
    T is N mod 2,
    place_piece(B, C, L, NB, T),
    NN is N-1,
    start_game(NN,NB).
    
