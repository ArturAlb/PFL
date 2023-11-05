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

## Conclusions

## Bibliography

https://boardgamegeek.com/boardgame/73091/momentum

https://www.youtube.com/watch?v=de_GZF4YSEE