% FILEPATH: /path/to/file.pl
:- use_module(library(lists)).
:- use_module(library(clpr)).
:- use_module(library(between)).
:- use_module(library(random)).

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
], [0, 0, 60, 0, 0]]).

set_player1(GameState, NewGameState,Player) :-
    nth1(2,GameState,Line),
    replace(Line,4,Player,NewLine),
    replace(GameState,2,NewLine,NewGameState).

set_player2(GameState, NewGameState,Player) :-
    nth1(2,GameState,Line),
    replace(Line,5,Player,NewLine),
    replace(GameState,2,NewLine,NewGameState).

choose_bot_type(Type):-
    write('Choose bot type:'),nl,
    write('1. Random'),nl,
    write('2. Greedy'),nl,
    read(Type).

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
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    read_option(Option),
    handle_option(Option).

read_option(Option) :-
    read(Option).

handle_option(1) :-
    write('You selected Option 1.\n'),
    initial_state(GameState),
    start_game(GameState).

handle_option(2) :-
    write('You selected Option 2.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player2(GameState, GameState1, Type),
    start_game(GameState1).

handle_option(3) :-
    write('You selected Option 3.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player1(GameState, GameState1, Type),
    start_game(GameState1).

handle_option(4) :-
    write('You selected Option 4.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player1(GameState, GameState1, Type),
    choose_bot_type(Type2),
    set_player2(GameState1, GameState2, Type2),
    start_game(GameState2).

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
    update_add_O(GameState, GameState1),
    get_Board(GameState1, Board),
    nth1(Line, Board, LineList),
    replace(LineList, Column, 'O', NewLineList),
    replace(Board, Line, NewLineList, NewBoard),
    update_Board(GameState1, NewBoard, NewGameState).

replace([_|T], 1, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

/*remove one piece from the board*/

remove_piece(GameState, Column, Line, NewGameState):-
    get_Board(GameState, Board),
    nth1(Line, Board, LineList),
    nth1(Column, LineList, Elem),
    place_piece(GameState, Column, Line, 2, NewGameState).

/*check if the game is over*/

get_player_type(GameState, Player):-
    get_Round(GameState, Round),
    Player1 is Round mod 2,
    Player2 is Player1 + 4,
    nth1(2,GameState,Line),
    nth1(Player2,Line,Player).

% game loop -----------------------------

start_game(GameState):-
    game_over(GameState, Winner),
    (Winner = 3 ->
        get_Round(GameState, Round),
        get_Board(GameState, Board),
        draw_board(Board),
        get_Xamount(GameState, Xpieces),
        get_Oamount(GameState, Opieces),
        write('Player 1 - X, player 2 - O'), nl,
        write('Player 1: '),write(Xpieces),nl,
        write('Player 2: '),write(Opieces),nl,
        get_player_type(GameState, Player),
        (Player = 0 ->
            check_move(GameState, GameState1)
            ;
            (Player = 1 -> 
                write('PC is thinking move'),nl,
                select_random_move(GameState, Col, Row),
                check_move(' ',GameState, Col, Row, GameState1)
                ;
                valid_moves(GameState, Moves),
                BaseMove = [1,1],
                get_best_move(Moves, GameState, BaseMove, 0, BestMove, BestValue),
                nth1(1,BestMove,Col),
                nth1(2,BestMove,Row),
                check_move(' ',GameState, Col, Row, GameState1),
                write('PC moved'),nl
                )
            
        ),
        update_Round(GameState1, GameState2),
        start_game(GameState2)
    ;
        end_screen(GameState, Winner)
    ).

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
    get_Board(GameState, B),
    write('Invalid, try again'),nl,
    write('Column: '),
    read(NC),nl,
    write('Line: '),
    read(NL),nl,
    nth1(NL,B,Line),
    nth1(NC,Line,Elem),
    check_move(Elem, GameState, NC, NL, NewGameState).

check_move('O', GameState, Column, Line, NewGameState):-
    get_Board(GameState, B),
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
    C > 0,
    L < 8,
    L > 0,
    get_Round(GameState, Round),
    Player is Round mod 2,
    NLD is L + 1,
    NLU is L - 1,
    NCR is C + 1,
    NCL is C - 1,
    move_right(GameState, NCR, L, GameState1),
    move_left(GameState1, NCL, L, GameState2),
    move_up(GameState2, C, NLU, GameState3),
    move_down(GameState3, C, NLD, GameState4),
    move_down_right(GameState4, NCR, NLD, GameState5),
    move_down_left(GameState5, NCL, NLD, GameState6),
    move_up_left(GameState6, NCL, NLU, GameState7),
    move_up_right(GameState7, NCR, NLU, GameState8),
    place_piece(GameState8, C, L, Player, NewGameState).

move_right(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (C = 8 ->
        NewGameState = GameState
    ;
        nth1(L,B,Line),
        nth1(C,Line,ElemC),
        (ElemC = ' ' ->
            NewGameState = GameState
        ;
            NC is C + 1,
            (NC = 8 ->
                remove_piece(GameState, C, L, GameState1),
                (ElemC = 'X' ->
                    update_take_X(GameState1, NewGameState)
                ;
                    update_take_O(GameState1, NewGameState)
                )
            ;
                nth1(L,B,Line1),
                nth1(NC,Line1,ElemR),
                (ElemR = ' ' ->
                    (ElemC = 'X' ->
                        place_piece(GameState, NC, L, 0, GameState1),
                        update_take_X(GameState1, GameState2)
                    ;
                        place_piece(GameState, NC, L, 1, GameState1),
                        update_take_O(GameState1, GameState2)
                    ),
                    remove_piece(GameState2, C, L, NewGameState)
                ;
                    move_right(GameState, NC, L, NewGameState)
                )
            )
        )
    ).

move_left(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (C = 0 ->
        NewGameState = GameState
    ;
        nth1(L,B,Line),
        nth1(C,Line,ElemC),
        (ElemC = ' ' ->
            NewGameState = GameState
        ;
            NC is C - 1,
            (NC = 0 ->
                remove_piece(GameState, C, L, GameState1),
                (ElemC = 'X' ->
                    update_take_X(GameState1, NewGameState)
                ;
                    update_take_O(GameState1, NewGameState)
                )
            ;
                nth1(L,B,Line1),
                nth1(NC,Line1,ElemR),
                (ElemR = ' ' ->
                    (ElemC = 'X' ->
                        place_piece(GameState, NC, L, 0, GameState1),
                        update_take_X(GameState1, GameState2)
                    ;
                        place_piece(GameState, NC, L, 1, GameState1),
                        update_take_O(GameState1, GameState2)
                    ),
                    remove_piece(GameState2, C, L, NewGameState)
                ;
                    move_left(GameState, NC, L, NewGameState)
                )
            )
        )
    ).

move_up(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 0 ->
        NewGameState = GameState
    ;
        nth1(L,B,Line),
        nth1(C,Line,ElemC),
        (ElemC = ' ' ->
            NewGameState = GameState
        ;
            NL is L - 1,
            (NL = 0 ->
                remove_piece(GameState, C, L, GameState1),
                (ElemC = 'X' ->
                    update_take_X(GameState1, NewGameState)
                ;
                    update_take_O(GameState1, NewGameState)
                )
            ;
                nth1(L,B,Line1),
                nth1(NC,Line1,ElemR),
                (ElemR = ' ' ->
                    (ElemC = 'X' ->
                        place_piece(GameState, C, NL, 0, GameState1),
                        update_take_X(GameState1, GameState2)
                    ;
                        place_piece(GameState, C, NL, 1, GameState1),
                        update_take_O(GameState1, GameState2)
                    ),
                    remove_piece(GameState2, C, L, NewGameState)
                ;
                    move_right(GameState, C, NL, NewGameState)
                )
            )
        )
    ).

move_down(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 8 ->
        NewGameState = GameState
    ;
        nth1(L,B,Line),
        nth1(C,Line,ElemC),
        (ElemC = ' ' ->
            NewGameState = GameState
        ;
            NL is L + 1,
            (NL = 8 ->
                remove_piece(GameState, C, L, GameState1),
                (ElemC = 'X' ->
                    update_take_X(GameState1, NewGameState)
                ;
                    update_take_O(GameState1, NewGameState)
                )
            ;
                nth1(NL,B,Line1),
                nth1(C,Line1,ElemR),
                (ElemR = ' ' ->
                    (ElemC = 'X' ->
                        place_piece(GameState, C, NL, 0, GameState1),
                        update_take_X(GameState1, GameState2)
                    ;
                        place_piece(GameState, C, NL, 1, GameState1),
                        update_take_O(GameState1, GameState2)
                    ),
                    remove_piece(GameState2, C, L, NewGameState)
                ;
                    move_down(GameState, C, NL, NewGameState)
                )
            )
        )
    ).

move_down_right(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 8 ->
        NewGameState = GameState
    ;
        (C = 8 ->
            NewGameState = GameState
        ;
            nth1(L,B,Line),
            nth1(C,Line,ElemC),
            (ElemC = ' ' ->
                NewGameState = GameState
            ;
                NL is L + 1,
                (NL = 8 ->
                    remove_piece(GameState, C, L, GameState1),
                    (ElemC = 'X' ->
                        update_take_X(GameState1, NewGameState)
                    ;
                        update_take_O(GameState1, NewGameState)
                    )
                ;
                    NC is C + 1,
                    (NC = 8 ->
                        remove_piece(GameState, C, L, GameState1),
                        (ElemC = 'X' ->
                            update_take_X(GameState1, NewGameState)
                        ;
                            update_take_O(GameState1, NewGameState)
                        )
                    ;
                        nth1(NL,B,Line1),
                        nth1(NC,Line1,ElemR),
                        (ElemR = ' ' ->
                            (ElemC = 'X' ->
                                place_piece(GameState, NC, NL, 0, GameState1),
                                update_take_X(GameState1, GameState2)
                            ;
                                place_piece(GameState, NC, NL, 1, GameState1),
                                update_take_O(GameState1, GameState2)
                            ),
                            remove_piece(GameState2, C, L, NewGameState)
                        ;
                            move_down_right(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

move_down_left(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 8 ->
        NewGameState = GameState
    ;
        (C = 0 ->
            NewGameState = GameState
        ;
            nth1(L,B,Line),
            nth1(C,Line,ElemC),
            (ElemC = ' ' ->
                NewGameState = GameState
            ;
                NL is L + 1,
                (NL = 8 ->
                    remove_piece(GameState, C, L, GameState1),
                    (ElemC = 'X' ->
                        update_take_X(GameState1, NewGameState)
                    ;
                        update_take_O(GameState1, NewGameState)
                    )
                ;
                    NC is C - 1,
                    (NC = 0 ->
                        remove_piece(GameState, C, L, GameState1),
                        (ElemC = 'X' ->
                            update_take_X(GameState1, NewGameState)
                        ;
                            update_take_O(GameState1, NewGameState)
                        )
                    ;
                        nth1(NL,B,Line1),
                        nth1(NC,Line1,ElemR),
                        (ElemR = ' ' ->
                            (ElemC = 'X' ->
                                place_piece(GameState, NC, NL, 0, GameState1),
                                update_take_X(GameState1, GameState2)
                            ;
                                place_piece(GameState, NC, NL, 1, GameState1),
                                update_take_O(GameState1, GameState2)
                            ),
                            remove_piece(GameState2, C, L, NewGameState)
                        ;
                            move_down_left(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

move_up_right(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 0 ->
        NewGameState = GameState
    ;
        (C = 8 ->
            NewGameState = GameState
        ;
            nth1(L,B,Line),
            nth1(C,Line,ElemC),
            (ElemC = ' ' ->
                NewGameState = GameState
            ;
                NL is L - 1,
                (NL = 0 ->
                    remove_piece(GameState, C, L, GameState1),
                    (ElemC = 'X' ->
                        update_take_X(GameState1, NewGameState)
                    ;
                        update_take_O(GameState1, NewGameState)
                    )
                ;
                    NC is C + 1,
                    (NC = 8 ->
                        remove_piece(GameState, C, L, GameState1),
                        (ElemC = 'X' ->
                            update_take_X(GameState1, NewGameState)
                        ;
                            update_take_O(GameState1, NewGameState)
                        )
                    ;
                        nth1(NL,B,Line1),
                        nth1(NC,Line1,ElemR),
                        (ElemR = ' ' ->
                            (ElemC = 'X' ->
                                place_piece(GameState, NC, NL, 0, GameState1),
                                update_take_X(GameState1, GameState2)
                            ;
                                place_piece(GameState, NC, NL, 1, GameState1),
                                update_take_O(GameState1, GameState2)
                            ),
                            remove_piece(GameState2, C, L, NewGameState)
                        ;
                            move_down_right(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

move_up_left(GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    (L = 0 ->
        NewGameState = GameState
    ;
        (C = 0 ->
            NewGameState = GameState
        ;
            nth1(L,B,Line),
            nth1(C,Line,ElemC),
            (ElemC = ' ' ->
                NewGameState = GameState
            ;
                NL is L - 1,
                (NL = 0 ->
                    remove_piece(GameState, C, L, GameState1),
                    (ElemC = 'X' ->
                        update_take_X(GameState1, NewGameState)
                    ;
                        update_take_O(GameState1, NewGameState)
                    )
                ;
                    NC is C - 1,
                    (NC = 0 ->
                        remove_piece(GameState, C, L, GameState1),
                        (ElemC = 'X' ->
                            update_take_X(GameState1, NewGameState)
                        ;
                            update_take_O(GameState1, NewGameState)
                        )
                    ;
                        nth1(NL,B,Line1),
                        nth1(NC,Line1,ElemR),
                        (ElemR = ' ' ->
                            (ElemC = 'X' ->
                                place_piece(GameState, NC, NL, 0, GameState1),
                                update_take_X(GameState1, GameState2)
                            ;
                                place_piece(GameState, NC, NL, 1, GameState1),
                                update_take_O(GameState1, GameState2)
                            ),
                            remove_piece(GameState2, C, L, NewGameState)
                        ;
                            move_down_left(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

valid_moves(GameState, Moves) :-
    get_Board(GameState, Board),
    findall(
        [Col, Row],
        (between(1, 7, Row), between(1, 7, Col), valid_move(GameState, Col, Row)),
        Moves
    ).

valid_move(GameState, Col, Row) :-
    Row > 0,
    Row < 8,
    Col > 0,
    Col < 8,
    get_Board(GameState, Board),
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    Elem = ' '.

select_random_move(GameState, Col,Row) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves),
    nth1(1,RandomMove,Col),
    nth1(2,RandomMove,Row).

end_screen(GameState, Winner):-
    (Winner = 0 ->
        write('Player 1 wins'),nl
    ;
        (Winner = 1 ->
            write('Player 2 wins'),nl
        ;
            write('it\'s a draw'),nl
        )
    ),
    get_Board(GameState, Board),
    draw_board(Board).

game_over(GameState, Winner):-
    get_Round(GameState, Round),
    get_Xamount(GameState, Xpieces),
    (Xpieces = 8 ->
        Winner = 0
    ;
        get_Oamount(GameState, Opieces),
        (Opieces = 8 ->
            Winner = 1
        ;
            (Round = 0 ->
                (Xpieces > Opieces ->
                    Winner = 0
                ;
                    (Opieces > Xpieces ->
                        Winner = 1
                    ;
                        Winner = 2
                    )
                )
            ;
                Winner = 3
            )
        )
    ).

get_gamestate_value(GameState, Value):-
    get_Xamount(GameState, Xpieces),
    get_Oamount(GameState, Opieces),
    get_Round(GameState, Round),
    Player is Round mod 2,
    (Player = 0 ->
        Value is Xpieces - Opieces
    ;
        Value is Opieces - Xpieces
    ).

get_best_move([H|T], GameState, BestMove, BestValue, NewBestMove, NewBestValue):-
    nth1(1,H,Col),
    nth1(2,H,Row),
    nth1(1,BestMove,BCol),
    nth1(2,BestMove,BRow),
    check_move(' ',GameState, Col, Row, NewGameState),
    get_gamestate_value(NewGameState, Value),
    (Value > BestValue ->
        NewBestValue1 is Value + 0,
        NewBestMove1 = H
        ;
        (Value = BestValue ->
            get_dist(Col, Row, Dist1),
            get_dist(BCol, BRow, Dist2),
            (Dist1 < Dist2 ->
                NewBestValue1 is Value + 0,
                NewBestMove1 = H
                ;
                NewBestValue1 is BestValue + 0,
                NewBestMove1 = BestMove
            )
            ;
            NewBestValue1 is BestValue + 0,
            NewBestMove1 = BestMove
        )
    ),
    get_best_move(T, GameState, NewBestMove1, NewBestValue1, NewBestMove, NewBestValue).

get_best_move([], GameState, BestMove, BestValue, NewBestMove, NewBestValue):-
    NewBestValue is BestValue + 0,
    NewBestMove = BestMove.

print_move(Move):-
    nth1(1,Move,Col),
    nth1(2,Move,Row),
    write('printMove: '),write(Col),write(' '),write(Row),nl.

get_dist(Col, Row, Dist):-
    Dist1 is 4 - Col,
    Dist2 is 4 - Row,
    Dist is sqrt(Dist1*Dist1 + Dist2*Dist2).