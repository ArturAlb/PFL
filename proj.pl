% FILEPATH: /path/to/file.pl
:- use_module(library(lists)).

% Define the initial state of the game
initial_state([
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ',' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
], [8, 8, 1, 30]).

/*draw a board of 7x7 with lines and columns*/

draw_board([H|T]) :-
    write('  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('  -----------------------------\n'),
    draw_board([H|T], 1).

draw_board(_, 7) :- !.

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

place_piece(Board, Player, Column, Line, NewBoard) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, Player, NewLineList),
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


