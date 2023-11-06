%checks if there are pieces to the right of the position (C,L) and moves the last one by 1 position in the same direction
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

%checks if there are pieces to the left of the position (C,L) and moves the last one by 1 position in the same direction
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

%checks if there are pieces above the position (C,L) and moves the last one by 1 position in the same direction
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
                    move_up(GameState, C, NL, NewGameState)
                )
            )
        )
    ).

%checks if there are pieces under the postion (C,L) and moves the last one by 1 position in the same direction
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

%checks if there are pieces to the above to the right of the position (C,L) and moves the last one by 1 position in the same direction
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

%checks if there are pieces down to the left of the position (C,L) and moves the last one by 1 position in the same direction
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

%checks if there are pieces above to the right of the position (C,L) and moves the last one by 1 position in the same direction
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
                            move_up_right(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

%checks if there are pieces above to the left of the position (C,L) and moves the last one by 1 position in the same direction
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
                            move_up_left(GameState, NC, NL, NewGameState)
                        )
                    )
                )
            )
        )
    ).

%gets the input from the user
move(GameState, NewGameState):-
    get_Board(GameState, B),
    write('Column: '),
    read(C),
    write('Line: '),
    read(L),
    nth1(L,B,Line),
    nth1(C,Line,Elem),
    move(Elem, GameState, C, L, NewGameState).

%asks for another position    
move('X',GameState, C, L, NewGameState):-
    get_Board(GameState, B),
    write('Invalid, try again'),nl,
    write('Column: '),
    read(NC),nl,
    write('Line: '),
    read(NL),nl,
    nth1(NL,B,Line),
    nth1(NC,Line,Elem),
    move(Elem, GameState, NC, NL, NewGameState).

%asks for another position
move('O', GameState, Column, Line, NewGameState):-
    get_Board(GameState, B),
    write('Invalid, try again'),nl,
    write('Column: '),
    read(NC),nl,
    write('Line: '),
    read(NL),nl,
    nth1(NL,B,Line),
    nth1(NC,Line,Elem),
    move(Elem, GameState, NC, NL, NewGameState).

%verifies the input and places the piece
move(' ', GameState, C, L, NewGameState):-
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
