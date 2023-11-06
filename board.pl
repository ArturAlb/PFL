%draws board header
draw_board([H|T]) :-
    write('  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n'),
    write('  -----------------------------\n'),
    draw_board([H|T], 1).

draw_board(_, 8) :- !.

%draws board
draw_board([H|T], N) :-
    write(N),
    write(' | '),
    draw_line(H),
    nl,
    write('  -----------------------------\n'),
    N1 is N + 1,
    draw_board(T, N1).

draw_line([]) :- !.

%draws the lines of the board
draw_line([H|T]) :-
    write(H),
    write(' | '),
    draw_line(T).

%removes one piece from the board
remove_piece(GameState, Column, Line, NewGameState):-
    get_Board(GameState, Board),
    nth1(Line, Board, LineList),
    nth1(Column, LineList, Elem),
    place_piece(GameState, Column, Line, 2, NewGameState).

%places an empty space on the board
place_piece(GameState, Column, Line, 2, NewGameState) :-
    get_Board(GameState, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, ' ', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState, NewBoard, NewGameState).

% places an 'X' piece on the board
place_piece(GameState, Column, Line, 0, NewGameState) :-
    update_add_X(GameState, GameState1),
    get_Board(GameState1, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'X', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState1, NewBoard, NewGameState).

%places an 'O' piece on the board
place_piece(GameState, Column, Line, 1, NewGameState) :-
    update_add_O(GameState, GameState1),
    get_Board(GameState1, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'O', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState1, NewBoard, NewGameState).

%replaces an element of a list
replace([_|T], 1, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).
