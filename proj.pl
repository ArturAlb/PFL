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

place_piece(Board, Column, Line, NewBoard, 2) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, ' ', NewLineList),
    replace(Board, Line, NewLineList, NewBoard).

place_piece(Board, Column, Line, NewBoard, 0) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'X', NewLineList),
    replace(Board, Line, NewLineList, NewBoard).

place_piece(Board, Column, Line, NewBoard, 1) :-
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'O', NewLineList),
    replace(Board, Line, NewLineList, NewBoard).

replace([_|T], 1, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

/*remove one piece from the board*/

remove_piece(Board, Column, Line, NewBoard) :-
    place_piece(Board, Column, Line, NewBoard, 2).

/*check if the game is over*/

is_even(X) :-
    0 is X mod 2.

start_game(0).

start_game(60):-
    initial_state(Board,Game_info),
    draw_board(Board),
    check_move(Board, 0, New_Board),
    Round is 60-1,
    start_game(Round,New_Board).

start_game(Round, Board):- Round>0,
    draw_board(Board),
    Player is Round mod 2,
    check_move(Board, Player, New_Board),
    Round1 is Round-1,
    start_game(Round1,New_Board).

    check_move(B,T,NB):-
        write('Column: '),
        read(C),
        write('Line: '),
        read(L),
        nth1(L,B,Line),
        nth1(C,Line,Elem),
        check_move(B,C,L,Elem,T,NB).
        
    check_move(B,C,L,'X',T,NB):-
        write('Invalid, try again'),nl,
        write('Column: '),
        read(NC),nl,
        write('Line: '),
        read(NL),nl,
        nth1(NL,B,Line),
        nth1(NC,Line,Elem),
        check_move(B,NC,NL,Elem,T,NB).
    
    check_move(B,C,L,'O',T,NB):-
        write('Invalid, try again'),nl,
        write('Column: '),
        read(NC),nl,
        write('Line: '),
        read(NL),nl,
        nth1(NL,B,Line),
        nth1(NC,Line,Elem),
        check_move(B,NC,NL,Elem,T,NB).
    
    check_move(B,C,L,' ',T,NB):-
        move_right(B,C,L,NNB),
        move_left(NNB,C,L,NNNB),
        place_piece(NNNB, C, L,NB, T),
        write('Valid'),nl.

move_right(B,C,L,NB):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,Elem),
    move_right(B,NC,L,Elem,NB).

move_right(B,C,L,' ',NB):-
    C < 7,
    write('Found a spot'),nl,
    NB = B.

move_right(B,C,L,'O',NNB):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_right(B,NC,L,NElem,NB),
    place_piece(NB,NC, L,NB1,1),
    remove_piece(NB1,C, L,NNB).

move_right(B,C,L,'X',NNB):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_right(B,NC,L,NElem,NB),
    place_piece(NB,NC, L,NB1,0),
    remove_piece(NB1,C, L,NNB).

move_left(B,C,L,NB):-
    C > 0,
    NC is C-1,
    nth1(L,B,Line),
    nth1(NC,Line,Elem),
    move_left(B,NC,L,Elem,NB).

move_left(B,C,L,' ',NB):-
    C > 0,
    write('Found a spot'),nl,
    NB = B.

move_left(B,C,L,'O',NNB):-
    C > 0,
    NC is C-1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_left(B,NC,L,NElem,NB),
    place_piece(NB,NC, L,NB1,1),
    remove_piece(NB1,C, L,NNB).

move_left(B,C,L,'X',NNB):-
    C > 0,
    NC is C-1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_left(B,NC,L,NElem,NB),
    place_piece(NB,NC, L,NB1,0),
    remove_piece(NB1,C, L,NNB).