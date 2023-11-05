% FILEPATH: /path/to/file.pl
:- use_module(library(lists)).

play :-
    menu.

% Define the initial state of the game
initial_state([[
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
], [0, 0, 60]]).

update_add_X(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(1,Line,XAmount),
    NewXAmount is XAmount + 1,
    replace(Line,1,NewXAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

update_take_X(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(1,Line,XAmount),
    NewXAmount is XAmount - 1,
    replace(Line,1,NewXAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

update_add_O(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(2,Line,OAmount),
    NewOAmount is OAmount + 1,
    replace(Line,2,NewOAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

update_take_O(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(2,Line,OAmount),
    NewOAmount is OAmount - 1,
    replace(Line,2,NewOAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

update_Round(GameState, NewGameState):-
    nth1(2,GameState, Line),
    nth1(3,Line, Round),
    NewRound is Round - 1,
    replace(Line, 3, NewRound, NewLine),
    replace(GameState, 2, NewLine, NewGameState).

get_Xamount(GameState, Xamount):-
    nth1(2,GameState,Line),
    nth1(1,Line,Xamount).

get_Oamount(GameState, Oamount):-
    nth1(2,GameState,Line),
    nth1(2,Line,Oamount).

get_Round(GameState, Round):-
    nth1(2,GameState, Line),
    nth1(3,Line, Round).

get_Board([Head|_], Head).

update_Board(GameState, Board, NewGameState):-
    replace(GameState, 1, Board, NewGameState).

/*menu*/

menu :-
    write('Menu:'), nl,
    write('1. 2 players'), nl,
    write('2. Option 2'), nl,
    write('3. Option 3'), nl,
    write('4. Option 4'), nl,
    read_option(Option),
    handle_option(Option).

read_option(Option) :-
    read(Option).

handle_option(1) :-
    write('You selected Option 1.\n'),
    start_game(60).

handle_option(2) :-
    write('You selected Option 2.\n').

handle_option(3) :-
    write('You selected Option 3.\n').

handle_option(4) :-
    write('You selected Option 4.\n').

handle_option(Option) :-
    write('Invalid option.'),
    menu.


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

place_piece(GameState, Column, Line, 2, NewGameState) :-
    get_Board(GameState, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, ' ', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState, NewBoard, NewGameState).

place_piece(GameState, Column, Line, 0, NewGameState) :-
    update_add_X(GameState, GameState1),
    get_Board(GameState1, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'X', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState1, NewBoard, NewGameState).

place_piece(GameState, Column, Line, 1, NewGameState) :-
    update_add_O(GameState1, GameState2),
    get_Board(GameState, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'O', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState, NewBoard, NewGameState).

replace([_|T], 1, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

/*remove one piece from the board*/

remove_piece(GameState, Column, Line, NewGameState):-
    nth1(Line, Board, LineList),
    nth1(Column, LineList, Elem),
    (Elem = 'X' ->
        update_take_X(GameState, GameState1)
    ;
        update_take_O(GameState, GameState1)
    ),
    place_piece(GameState, Column, Line, NewGameState, 2).

/*check if the game is over*/

is_even(X) :-
    0 is X mod 2.

% game loop -----------------------------

start_game(0).

start_game(60):-
    initial_state(GameState),
    get_Board(GameState, Board),
    draw_board(Board),
    get_Xamount(GameState, Xpieces),
    get_Oamount(GameState, Opieces),
    write('Player 1 - X, player 2 - O'), nl,
    write('Player 1: '),write(Xpieces),nl,
    write('Player 2: '),write(Opieces),nl,
    check_move(GameState, GameState1),
    update_Round(GameState1, GameState2),
    start_game(GameState2).

start_game(GameState):-
    get_Round(GameState, Round),
    get_Board(GameState, Board),
    Round > 0,
    draw_board(Board),
    get_Xamount(GameState, Xpieces),
    get_Oamount(GameState, Opieces),
    write('Player 1: '),write(Xpieces),nl,
    write('Player 2: '),write(Opieces),nl,
    check_move(GameState, GameState1),
    update_Round(GameState1, GameState2),
    start_game(GameState2).

check_move(GameState, NewGameState):-
    get_Board(GameState, B),
    write('Column: '),
    read(C),
    write('Line: '),
    read(L),
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    check_move(Elem, GameState, C, L, NewGameState).
    
check_move('X',GameState, C, L, NewGameState):-
    write('Invalid, try again'),nl,
    write('Column: '),
    read(NC),nl,
    write('Line: '),
    read(NL),nl,
    nth1(NL,B,Line),
    nth1(NC,Line,Elem),
    check_move(Elem, GameState, NC, NL, NewGameState).

check_move('O', GameState, Column, Line, NewGameState):-
    write('Invalid, try again'),nl,
    write('Column: '),
    read(NC),nl,
    write('Line: '),
    read(NL),nl,
    nth1(NL,B,Line),
    nth1(NC,Line,Elem),
    check_move(Elem, GameState, NC, NL, NewGameState).

check_move(' ', GameState, C, L, NewGameState):-
    C < 8,
    get_Round(GameState, Round),
    Player is Round mod 2,
    NLD is L + 1,
    NLU is L - 1,
    NCR is C + 1,
    NCL is C - 1,
    move_right(GameState, NCR, L, GameState1),
    place_piece(GameState1, C, L, Player, NewGameState),
    write('Valid'),nl.

move_right(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NewGameState = GameState
    ;
        NC is C + 1,
        (NC = '8' ->
            remove_piece(GameState, C, L, NewGameState)
        ;
            nth1(L,B,Line1),
            nth1(NC,Line1,ElemR),
            (ElemR = ' ' ->
                (ElemC = 'X' ->
                    place_piece(GameState, NC, L, 0, GameState1)
                ;
                    place_piece(GameState, NC, L, 1, GameState1)
                ),
                remove_piece(GameState1, C, L, NewGameState)
            ;
                move_right(GameState, NC, L, NewGameState)
            )
        )
    ).

move_left(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NC is C - 1,
    (NC = '0' ->
        remove_piece(GameState, C, L, NewGameState)
    ;
        nth1(L,B,Line),
        nth1(NC,Line,ElemR),
        (ElemC = ' ' ->
            write('cenas'),nl,
            NB = B,
            Xnewpieces = Xpieces,
            Onewpieces = Opieces
        ;
            (ElemR = ' ' ->
                place_piece(GameState, NC, L, GameState1),
                remove_piece(GameState1, C, L, NewGameState)
            ;
                move_right(GameState, NC, L, NewGameState)
            )
        )
    ).

move_up(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L - 1,
    nth1(NL,B,Line1),
    nth1(C,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,C,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,C,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_up(B,C,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).

move_down(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L + 1,
    nth1(NL,B,Line1),
    nth1(C,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,C,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,C,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_down(B,C,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).

move_up_right(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L - 1,
    NC is C + 1,
    nth1(NL,B,Line1),
    nth1(NC,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,NC,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,NC,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_up_right(B,NC,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).

move_up_left(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L - 1,
    NC is C - 1,
    nth1(NL,B,Line1),
    nth1(NC,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,NC,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,NC,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_up_left(B,NC,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).

move_down_right(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L + 1,
    NC is C + 1,
    nth1(NL,B,Line1),
    nth1(NC,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,NC,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,NC,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_down_right(B,NC,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).

move_down_left(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    nth1(L,B,Line),
    nth1(C,Line,ElemC),
    NL is L + 1,
    NC is C - 1,
    nth1(NL,B,Line1),
    nth1(NC,Line1,ElemR),
    (ElemC = ' ' ->
        write('cenas'),nl,
        NB = B,
        Xnewpieces = Xpieces,
        Onewpieces = Opieces
    ;
        (ElemR = ' ' ->
            (ElemC = 'X' ->
                place_piece(B,NC,NL,NB1,0,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'X',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            ;
                place_piece(B,NC,NL,NB1,1,Xpieces,Opieces,Xnewpieces1, Onewpieces1),
                remove_piece(NB1,C,L,NB,'O',Xnewpieces1,Onewpieces1,Xnewpieces, Onewpieces)
            )
        ;
            move_down_left(B,NC,NL,NB,Xpieces,Opieces,Xnewpieces,Onewpieces)
        )
    ).




/*
move_right(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,Elem),
    move_right(B,NC,L,Elem,NB,Xpieces, Opieces,Xnewpieces, Onewpieces).

move_right(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    C < 7,
    write('Found a spot'),nl,
    Xnewpieces is Xpieces-0,
    Onewpieces is Opieces-0,
    NB = B.

move_right(B,C,L,'O',NNB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_right(B,NC,L,NElem,NB,Xpieces, Opieces,Xnewnewpieces, Onewnewpieces),
    place_piece(NB,NC, L,NB1,1,Xnewnewpieces, Onewnewpieces,Xnewnewnewpieces, Onewnewnewpieces),
    remove_piece(NB1,C, L,NNB,'O',Xnewnewnewpieces, Onewnewnewpieces,Xnewpieces, Onewpieces).

move_right(B,C,L,'X',NNB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    C < 7,
    NC is C+1,
    nth1(L,B,Line),
    nth1(NC,Line,NElem),
    move_right(B,NC,L,NElem,NB,Xpieces, Opieces,Xnewnewpieces, Onewnewpieces),
    place_piece(NB,NC, L,NB1,0,Xnewnewpieces, Onewnewpieces,Xnewnewnewpieces, Onewnewnewpieces),
    remove_piece(NB1,C, L,NNB,'X',Xnewnewnewpieces, Onewnewnewpieces,Xnewpieces, Onewpieces).

move_right(B,7,L,'O',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,7, L,NB,'O',Xpieces, Opieces,Xnewpieces, Onewpieces).
    

move_right(B,7,L,'X',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,7, L,NB,'X',Xpieces, Opieces,Xnewpieces, Onewpieces).

move_right(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    write('Found a spot'),nl,
    Xnewpieces = Xpieces,
    Onewpieces = Opieces,
    NB = B.

move_up(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L > 1,
    NL is L-1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_up(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces, Onewpieces).

move_up(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L > 1,
    write('Found a spot'),nl,
    Xnewpieces = Xpieces,
    Onewpieces = Opieces,
    NB = B.

move_up(B,C,L,'O',NB2,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L > 1,
    NL is L-1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_up(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces1, Onewpieces1),
    place_piece(NB,C, NL,NB1,1,Xnewpieces1, Onewpieces1,Xnewpieces2, Onewpieces2),
    remove_piece(NB1,C, L,NB2,'O',Xnewpieces2, Onewpieces2,Xnewpieces, Onewpieces).

move_up(B,C,L,'X',NB2,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L > 1,
    NL is L-1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_up(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces1, Onewpieces1),
    place_piece(NB,C, NL,NB1,0,Xnewpieces1, Onewpieces1,Xnewpieces2, Onewpieces2),
    remove_piece(NB1,C, L,NB2,'X',Xnewpieces2, Onewpieces2,Xnewpieces, Onewpieces).

move_up(B,C,1,'O',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,C, 1,NB,'O',Xpieces, Opieces,Xnewpieces, Onewpieces).

move_up(B,C,1,'X',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,C, 1,NB,'X',Xpieces, Opieces,Xnewpieces, Onewpieces).

move_up(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    write('Found a spot'),nl,
    Xnewpieces = Xpieces,
    Onewpieces = Opieces,
    NB = B.

move_down(B,C,L,NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L < 7,
    NL is L+1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_down(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces, Onewpieces).

move_down(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L < 7,
    write('Found a spot'),nl,
    Xnewpieces = Xpieces,
    Onewpieces = Opieces,
    NB = B.

move_down(B,C,L,'O',NB2,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L < 7,
    NL is L+1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_down(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces1, Onewpieces1),
    place_piece(NB,C, NL,NB1,1,Xnewpieces1, Onewpieces1,Xnewpieces2, Onewpieces2),
    remove_piece(NB1,C, L,NB2,'O',Xnewpieces2, Onewpieces2,Xnewpieces, Onewpieces).

move_down(B,C,L,'X',NB2,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    L < 7,
    NL is L+1,
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    nth1(NL,B,NLine),
    nth1(C,NLine,NElem),
    move_down(B,C,NL,NElem,NB,Xpieces, Opieces,Xnewpieces1, Onewpieces1),
    place_piece(NB,C, NL,NB1,0,Xnewpieces1, Onewpieces1,Xnewpieces2, Onewpieces2),
    remove_piece(NB1,C, L,NB2,'X',Xnewpieces2, Onewpieces2,Xnewpieces, Onewpieces).

move_down(B,C,7,'O',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,C, 7,NB,'O',Xpieces, Opieces,Xnewpieces, Onewpieces).

move_down(B,C,7,'X',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    remove_piece(B,C, 7,NB,'X',Xpieces, Opieces,Xnewpieces, Onewpieces).

move_down(B,C,L,' ',NB,Xpieces, Opieces,Xnewpieces, Onewpieces):-
    write('Found a spot'),nl,
    Xnewpieces = Xpieces,
    Onewpieces = Opieces,
    NB = B.
*/