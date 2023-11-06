%menu
menu :-
    write('Menu:'), nl,
    write('1. H/H'), nl,
    write('2. H/PC'), nl,
    write('3. PC/H'), nl,
    write('4. PC/PC'), nl,
    write('Choose an option: '),
    read_option(Option),
    handle_option(Option).

%reads the input from the user
read_option(Option) :-
    read(Option).

%handles the input if the first option is chosen
handle_option(1) :-
    write('You selected Option 1.\n'),
    initial_state(GameState),
    start_game(GameState).

%handles the input if the second option is chosen
handle_option(2) :-
    write('You selected Option 2.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player2(GameState, GameState1, Type),
    start_game(GameState1).

%handles the input if the third option is chosen
handle_option(3) :-
    write('You selected Option 3.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player1(GameState, GameState1, Type),
    start_game(GameState1).

%handles the input if the fourth option is chosen
handle_option(4) :-
    write('You selected Option 4.\n'),
    initial_state(GameState),
    choose_bot_type(Type),
    set_player1(GameState, GameState1, Type),
    choose_bot_type(Type2),
    set_player2(GameState1, GameState2, Type2),
    start_game(GameState2).

%handles the input if an invalid number is inputed
handle_option(Option) :-
    write('Invalid option.'),
    menu.

%chooses the bot (Random or Greedy)
choose_bot_type(Type):-
    write('Choose bot type:'),nl,
    write('1. Random'),nl,
    write('2. Greedy'),nl,
    read(Type).
