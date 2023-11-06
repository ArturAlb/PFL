% FILEPATH: /path/to/file.pl
:- use_module(library(lists)).
:- use_module(library(clpr)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult('board.pl').
:- consult('computers.pl').
:- consult('gamestate.pl').
:- consult('menu.pl').
:- consult('moves.pl').

%play function
play :-
    menu.

% game loop
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
                get_best_move(Moves, GameState, BaseMove, -10, BestMove, BestValue),
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

%checks if there is a winner or if the 60 rounds have passed, checks who has the most pieces on the board
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

%shows final board and winner or draw
end_screen(GameState, Winner):-
    get_Board(GameState, Board),
    draw_board(Board),
    (Winner = 0 ->
        write('Player 1 wins'),nl
    ;
        (Winner = 1 ->
            write('Player 2 wins'),nl
        ;
            write('it\'s a draw'),nl
        )
    ),
    menu.
