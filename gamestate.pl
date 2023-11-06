%initial_state
initial_state([[
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
], [0, 0, 60, 0, 0]]).

%sets a value to player 1 (0 - Human, 1 - Random_bot, 2 - Greedy bot)
set_player1(GameState, NewGameState,Player) :-
    nth1(2,GameState,Line),
    replace(Line,4,Player,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%sets a value to player 2 (0 - Human, 1 - Random_bot, 2 - Greedy bot)
set_player2(GameState, NewGameState,Player) :-
    nth1(2,GameState,Line),
    replace(Line,5,Player,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%adds 1 to the counter of 'X' pieces on the board
update_add_X(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(1,Line,XAmount),
    NewXAmount is XAmount + 1,
    replace(Line,1,NewXAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%substracts 1 to the counter of 'X' pieces on the board
update_take_X(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(1,Line,XAmount),
    NewXAmount is XAmount - 1,
    replace(Line,1,NewXAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%adds 1 to the counter of 'O' pieces on the board
update_add_O(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(2,Line,OAmount),
    NewOAmount is OAmount + 1,
    replace(Line,2,NewOAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%substracts 1 to the counter of 'X' pieces on the board
update_take_O(GameState,NewGameState) :-
    nth1(2,GameState,Line),
    nth1(2,Line,OAmount),
    NewOAmount is OAmount - 1,
    replace(Line,2,NewOAmount,NewLine),
    replace(GameState,2,NewLine,NewGameState).

%substracts 1 to the number of rounds left
update_Round(GameState, NewGameState):-
    nth1(2,GameState, Line),
    nth1(3,Line, Round),
    NewRound is Round - 1,
    replace(Line, 3, NewRound, NewLine),
    replace(GameState, 2, NewLine, NewGameState).

%gives the number of 'X' on the board
get_Xamount(GameState, Xamount):-
    nth1(2,GameState,Line),
    nth1(1,Line,Xamount).

%gives the number of 'O' on the board
get_Oamount(GameState, Oamount):-
    nth1(2,GameState,Line),
    nth1(2,Line,Oamount).

%gives the number of rounds left
get_Round(GameState, Round):-
    nth1(2,GameState, Line),
    nth1(3,Line, Round).

%takes out the board from the GameState
get_Board([Head|_], Head).

%updates the board inside the GameState
update_Board(GameState, Board, NewGameState):-
    replace(GameState, 1, Board, NewGameState).

%returns the type of player (0 - Human, 1 - Random_bot, 2 - Greedy bot)
get_player_type(GameState, Player):-
    get_Round(GameState, Round),
    Player1 is Round mod 2,
    Player2 is Player1 + 4,
    nth1(2,GameState,Line),
    nth1(Player2,Line,Player).
