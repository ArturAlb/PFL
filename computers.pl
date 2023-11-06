%gives all valid_moves
valid_moves(GameState, Moves) :-
    get_Board(GameState, Board),
    findall(
        [Col, Row],
        (between(1, 7, Row), between(1, 7, Col), valid_move(GameState, Col, Row)),
        Moves
    ).

%checks if a position is empty
valid_move(GameState, Col, Row) :-
    Row > 0,
    Row < 8,
    Col > 0,
    Col < 8,
    get_Board(GameState, Board),
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    Elem = ' '.

%give a value to a given GameState
value(GameState, Value):-
    get_Xamount(GameState, Xpieces),
    get_Oamount(GameState, Opieces),
    get_Round(GameState, Round),
    Player is Round mod 2,
    (Player = 0 ->
        Value is Xpieces - Opieces
    ;
        Value is Opieces - Xpieces
    ).

%gives the best move playable
get_best_move([H|T], GameState, BestMove, BestValue, NewBestMove, NewBestValue):-
    nth1(1,H,Col),
    nth1(2,H,Row),
    nth1(1,BestMove,BCol),
    nth1(2,BestMove,BRow),
    move(' ',GameState, Col, Row, NewGameState),
    value(NewGameState, Value),
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

%gives the the sum of the difference of the row and the col to the center
get_dist(Col, Row, Dist):-
    Dist1 is abs(4 - Col),
    Dist2 is abs(4 - Row),
    Dist is Dist1+Dist2.

%selects a random_move from all the valid_moves
select_random_move(GameState, Col,Row) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves),
    nth1(1,RandomMove,Col),
    nth1(2,RandomMove,Row).
