# Momentum
>       Group: Momentum_2

>   (50%) up202006479 - Ruben Silverio Fernandes Esteves

>   (50%) up202108663 - Artur Jose Albuquerque Oliveira

## Instalattion and Execution

In order to play this game, you first need to have installed SICStus Prolog 4.8 or a newer version and downloaded the folder with the source code.

Next, open SICStus Prolog and type the following line to consult the proj.pl file:

```
?- consult('your_directory/proj.pl').
```

All you need to do now is star the game with:
```
?- play.
```
## Game Description
 
The game needs 2 players, a Board and 8 pieces for each player.

### Board and Pieces

The board is squared and consists of a 7x7 grid. Each player starts with 8 pieces (X or O).

### Gameplay

The players take turns placing a single piece each round, trying to get all of their 8 pieces on the board simultaneously. Why isn't this so staightforward? Each time a player places a piece next to another piece, it's momentum is transfered in a line in every direction displacing the piece at the end of the line.

Take the following board as an example:

```
  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
  -----------------------------
1 |   |   |   |   |   | X |   | 
  -----------------------------
2 |   |   |   |   |   |   |   | 
  -----------------------------
3 |   |   |   |   |   | O |   | 
  -----------------------------
4 |   |   | X | O |   |   |   | 
  -----------------------------
5 |   |   |   |   |   |   |   | 
  -----------------------------
6 |   |   |   |   |   |   |   | 
  -----------------------------
7 |   |   |   |   |   |   |   | 
  -----------------------------
```
Now, player 2 will place their "O" pice in (5,4). Let's see what happens:

```
  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
  -----------------------------
1 |   |   |   |   |   | X |   | 
  -----------------------------
2 |   |   |   |   |   |   | O | 
  -----------------------------
3 |   |   |   |   |   |   |   | 
  -----------------------------
4 |   | X |   | O | O |   |   | 
  -----------------------------
5 |   |   |   |   |   |   |   | 
  -----------------------------
6 |   |   |   |   |   |   |   | 
  -----------------------------
7 |   |   |   |   |   |   |   | 
  -----------------------------
```

It pushed away th "X" and "O" pieces.
If a piece is on the edge of the board and gets pushed away, it is knocked off the board and goes back to it's respective player's hand.

### Win Condition

The game ends when one of the players has 8 pieces on the board or 60 rounds have passed (the winner being the one with most pieces on the board).

## Game logic
### Internal game state representations

The state of the game is represented by a list containing the board and a second list containing information inportant to the game.
The board consists of 7 lists (rows) of 7 elements (columns).
The second list consist of 5 elements:
> 1. The amount of X pieces
> 2. The amount of O pieces
> 3. The current round
> 3. Player 1's type
> 5. Player 2's type

Player can be 0 -> Human, 1 -> Random bot and 2 -> Greedy bot
```prolog
[[
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
], [0, 0, 60, 0, 0]]
```
### Game State Visualization
Upon starting the game with 
```
?- play.
```
Players are brought to this menu where they can choose which type of game they want to play.
```
Menu:
1. 2 players
2. H/PC
3. PC/H
4. PC/PC
|: 
```
```prolog
menu :-
    write('Menu:'), nl,
    write('1. 2 players'), nl,
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    read_option(Option),
    handle_option(Option).
```
If a player chooses a mode where there are computer players they then have to choose which kind of computer they want to play against:
```
You selected Option 2.
Choose bot type:
1. Random
2. Greedy
|: 
```
Upon starting the game and after each play, the board is displayed as follows:
```
  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
  -----------------------------
1 |   |   |   |   |   |   |   | 
  -----------------------------
2 |   |   |   |   |   |   |   | 
  -----------------------------
3 |   |   |   |   |   |   |   | 
  -----------------------------
4 |   |   |   |   |   |   |   | 
  -----------------------------
5 |   |   |   |   |   |   |   | 
  -----------------------------
6 |   |   |   |   |   |   |   | 
  -----------------------------
7 |   |   |   |   |   |   |   | 
  -----------------------------
Player 1 - X, player 2 - O
Player 1: 0
Player 2: 0
```
This is an example of a game in a later stage:
```
  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
  -----------------------------
1 |   |   |   |   |   |   |   | 
  -----------------------------
2 |   | O | X | O | X |   | O | 
  -----------------------------
3 |   |   |   |   |   |   |   | 
  -----------------------------
4 | O | X |   | O | X | X |   | 
  -----------------------------
5 |   |   |   |   |   |   |   | 
  -----------------------------
6 |   |   |   | X |   | X |   | 
  -----------------------------
7 |   |   |   | O |   |   |   | 
  -----------------------------
Player 1 - X, player 2 - O
Player 1: 7
Player 2: 6
```

### Move Validation and Execution

The move validation is done by the check_move predicate, which checks if a input is within the bounds of the board and if the space is free:

```prolog
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
```
In case its free, check_move will call the predicates to move pieces next to placed one:
```prolog
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
```
Here is an example of one of those predicates:
```prolog
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
```
These predicates will recursively search for an empty space in a straight line and move the last piece connected to the one placed.

### List of Valid Moves
```prolog
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
```
The valid_move predicate checks if a space is within the bounds and empty and valid_moves uses it to find all valid move, returning a list of lists in Moves.

### End of Game

The game_over predicate checks if any of the players has placed 8 pieces and if the number of remaining rounds has reached 0 and decides the winner.
```prolog
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
```

### Game State Evaluation
The game sate evaluation is done by subtracting the amount of pieces the enemy has placed and comparing to the players.
```prolog
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
```

### Computer Plays
We have two types of computers:

The one that plays randomly:
```prolog
select_random_move(GameState, Col,Row) :-
    valid_moves(GameState, Moves),
    random_member(RandomMove, Moves),
    nth1(1,RandomMove,Col),
    nth1(2,RandomMove,Row).
```
Randomly selecting a move from the list of valid moves.
And a greedy algorithm powered one:
```prolog
get_best_move([H|T], GameState, BestMove, BestValue, NewBestMove, NewBestValue):-
    nth1(1,H,Col),
    nth1(2,H,Row),
    nth1(1,BestMove,BCol),
    nth1(2,BestMove,BRow),
    check_move(' ',GameState, Col, Row, NewGameState),
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

get_dist(Col, Row, Dist):-
    Dist1 is abs(4 - Col),
    Dist2 is abs(4 - Row),
    Dist is Dist1+Dist2.
```
This algorithm checks all the valid moves and calculates the value of the game state after each move. In case of multiple moves that generate the same value, this algorithm will try to play closest to the center of the board in order to be more defensive.

## Conclusions
We had some struggle trying to make this project due to the lack of loops and other limitations that arent present in most languages so it really helped us develop our skills in areas where we werent so profficient.
## Bibliography

https://boardgamegeek.com/boardgame/73091/momentum

https://www.youtube.com/watch?v=de_GZF4YSEE