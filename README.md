# YOTE
T03_Yote5

* Martim Videira - up202006289    (50%)
* Miguel Silva  - up202007972     (50%)

### TABLE OF CONTENTS

- [Installation and Execution](#installation-and-execution)
- [Game description](#game-description) 
- [Game Logic](#game-logic)
    - [Internal representation of the game state](#internal-representation-of-the-game-state)
    - [Game state visualization](#game-state-visualization)
    - [Execution of moves](#execution-of-moves)
    - [Valid plays](#valid-plays)
    - [Game over](#game-over)
    - [Board evaluation](#board-evaluation)
    - [PC's moves](#pc-move)
- [Conclusions](#conclusions)
- [Bibliography](#bibliography)


## Instalation and Execution

To execute our game it's necessary to have SWI-Prolog installed and game's source code. Then, to play the game, the user must run the following commands in the Prolog environment:
```
?- consult('./play.pl').
?- play.
```

## Game Description

Yote is a traditional board game from West Africa, which main goal is to capture all of the pieces of the other player. It is fast-paced and can have unexpected turnarounds, that we thought that would  be interesting to see implemented in PROLOG. 


![](https://i.imgur.com/ieqmjwN.png)
*Fig 1 - Image of the empty board, at the begining of the game from https://www.di.fc.ul.pt/~jpn/gv/yote.htm*

### Rules
This game is originally played on an empty 5x6 square board, where each player has 12 stones offboard. On each turn, each player may either **deploy a stone** from the reserve to any empty space on the board, or **move a stone on the board**.
* Stones can move by sliding or capturing an opponent stone by jumping over it. 
* The game is won by the player who captures all of their opponent's stones. 
#### Optional Rules
These rules may also be applied when playing a game of Yote.

* When a stone captures another stone, the player can capture any other opponent stone on the board (but not in their opponent's reserve).
* A player can continue to jump enemy stones until there are no more stones to jump, but captures are not mandatory.
* If either player is unable to make a move, the game ends and the player with the most stones remaining wins. 
* If both players have three or fewer stones remaining, the game is a draw.


### Game Modes
We decided to develop various game modes of different kinds:

* **Multiplayer** ( Players vs Player) - Two human players (users) face off against each other.
* **Face Dumb AI** (Player vs Dumb AI) - The user plays against our dumbest AI, that randomly selects each move it plays.
* **Face Greedy AI** (Player vs Greedy AI) - The user plays against our greedy AI, an AI that selects the best local move every single time. 
* **Face Minimax AI** (Player vs Minimax AI) - The user plays against our Minimax AI, an AI that selects the best move according to a Minimax search done on the given state of the Board. 
* **Watch Minimax vs Minimax** (Minimax AI vs Minimax AI) - The user watches a Minimax AI face off against the same version of itself.
* **Watch Minimax vs Greedy** (Minimax AI vs Greedy AI) -  The user watches a Minimax AI face off against the previously described Greedy AI.
* **Watch Minimax vs Dumb** (Minimax AI vs Dumb AI) -  The user watches a Minimax AI face off against the previously described Dumb AI.

To do so, as stated before, we have implemented 3 diferent AIs:
*    **Dumb AI**   - an AI selects its moves randomly;
*    **Greedy AI** - an AI that selects the best local move every single time, based on the greedy algorithm taught in the theorical lessons. 
*    **Minimax AI** - An AI that selects the best move according to a Minimax search done on the given state of the Board. 

## Game Logic
### Internal Representation of the Game State

Even though the game is originally played on a 5x6 board, we have implemented the option for the **user to alter the size of the board** in the game's menu. That way, the user can freely stretch the board as he pleases and change the number of pieces played per game.


![](https://i.imgur.com/9ccde7D.png)
*Fig 2 - Screenshot of the initial Game State as it was originally played *


![](https://i.imgur.com/0TwFW6O.png)
*Fig 3 - Screenshot of the initial Game State, with a different size chosen by the user. In this case, a  9x9 board*

With that explained, our Game State is made out of 3 things:
* Board - Representation of our game board in a list of lists format
* Player - the player that shall play in the given round,
* 1Pieces - the pieces that player 1 has in hand, and the ones that have been captured up until that point, in the format **[inHandPieces, capturedPieces]**
* 2Pieces - the pieces that player 2 has in hand, and the ones that have been captured up until that point, in the format **[inHandPieces, capturedPieces]**

Based on this representation, our game state looks as follows
* **State** = [Board, Player, 1Pieces, 2Pieces]

To display the pieces that are being played, we put a **"O"** on the board to represent player 1's pieces, and a **"X"** to represent player 2's pieces. The empty cells remain as blank spaces - **" "** - until it is occupied.

By playing the game, you may reach a Game State similar to the one depicted below:

![Intermediate State](https://i.imgur.com/93mmius.png)
*Fig 4 - Screenshot of an intermediate Game State, played on a 5x6 board with 12 pieces on each player.*

And possible final game state given the logic of Yote is as follows:
![](https://i.imgur.com/mqNkR8W.png)
*Fig 5 - Screenshot of an final Game State, played on a 4x4 board with 2 pieces on each player. In the last play, player 1 captures the opponent's piece on the position 'b4', using the move 'a4c4'. With that, player 1 captures all the opponent's pieces, winning the game.*

### Game state visualization

After running *play/0* predicate, the game starts by displaying the main menu. There, the user may choose from the different options and leave the program if they wish to. We have also an option for the user to configure the board and pieces, as stated before:

![](https://i.imgur.com/wAyLD7K.png)
*Fig 6 - The Game Menu*

To display the menu, the predicate *menu/0* is invoked. This predicate is responsible for calling the different '*play*' methods, based on the selected game mode. Each of these methods then calls **display_game(+ [Board,_Player,[Player1Pieces,Player1Captured],[Player2Pieces,Player2Captured]]** - which is the predicate that receives the Game State and prints to the terminal the board and players' pieces.

The Game State that is passed to the *display_game* predicate is the one instantiated by the predicate **initial_state(+ [Board,player1,[N,0],[N,0]]))** - method that creates the board based on the configuration selected by the player, initially.

This is achieved with the use of Helper functions, defined in the source files **utils.pl** and **io.pl**. Predicates like **myRepeat/3** help iterate through the board and create its initial state. The predicate **printBoard(+ Board)** takes solely a given board and writes it to the console, being part of the *display_game* predicate.

There also two predicates (**aiBoard/0** and **normalBoard/0**) that clear the game's configuration and set the board to a new specification of columns and lines. This is used by the menu to change the game state's internal representation, and therefore, its visualization.

### Execution of moves

In Yote, there two types of moves:
* Placing a piece on a empty cell
* Moving a piece from one cell to another

With that said, it is also important to mention that each and every cell follows the notation **"ColumnLine"**, with column being a character in this case. An example would be "b5" - the cell located in the 2nd column, fifth row.

#### To place a move:

To place a move simply specify the position of the empty cell you're trying to occupy. Examples:
* 'b5'
* 'c4'
* 'a1'

#### To move a piece:

To move a piece you need to specify the starting position of the piece you're trying to move and its final position. Examples:
* 'b5c5'
* 'c4a4'
* 'a1b1'

This action is performed by the **move(+ [Board,Player|Rest],+[C,L], - NewState)** predicate, that receives a state ([Board,Player|Rest]) and piece to be played (in this case, placed on the position 'CL') and returns the modified board via the argument NewState
```prolog
move([Board,Player|Rest],[C,L],NewState):-
    at(L,Board,Line),
    piece(Player,PlayerPiece),
    setAt(C,Line,PlayerPiece,NewLine),
    setAt(L,Board,NewLine,NewBoard),
    decrement_hand_pieces([NewBoard,Player|Rest],NewState).
```
Using the *seAt/3* predicate, we place a piece at Line and Column specified, and we alter it on the NewState variable we need to return. To reach this function, the movement that player chose first needs to be validated and checked, which will be explained in the next section.

### Valid plays

After the player gives their input, the chosen movement is then passed on the predicate **validatePlayerMove(+ State, + Move)**. This predicate will then call **isValidMove(State,[+ C, + L])**, the predicate that checks if the move given by the player is valid or not. It checks if we are moving a piece of our own, or if we are placing a new piece on a empty cell, etc. After performing all those validations, *isValidMove* will either call **whyNotValid(+ _State, +[C,L])** - a predicate that prints to the console why the chosen movement is not valid - or terminate and backtrack.

Some of the mentioned validations also happen at the **validPosition(+ C, + L)** predicate, which is called by *isValidMove*. There, the selected move will be tested to check if the cell selected is out of bounds and respects the size of the board.

### Game Over

The game ends when one player has captured all the other player's pieces. To validate that win condition, we call our predicate **game_over(+ [Board,_|Rest], + player)** that checks precisely that.

### Board evaluation

The predicate **value(+ [Board, _|Rest], - Score)** calculates the score given a certain board. It is a fairly simple method, as it only takes into account the number of pieces captured by each player. With that said, if the player 2 has captured more pieces than player 1, the score is **positive**, otherwise it would be **negative or null**.

### PC's moves

As stated before, we developed three different AIs for this projects. The code process for them to choose a move is as follows:
* Dumb AI Move: randomly selects a move out of all the possible moves
* Greedy AI Move: uses predicate **greedy_choice/3** to first create a set of possible moves, ordered by the score of the resulting Board, by calling the predicate **value/2**. After doing so, chooses the one with the poorest score (being the first in the set).
* Minimax AI Move: uses predicate **minimax_choice/2** to perform a Min-Max search on the given Game State. Then, stores the best moves and values for each Board while traversing the tree of Boards in a set of rules. Then, retrieves the best given move for the Game State that was presented initially. To evaluate the positions once the **Depth** variable of the search hits zero, we use the predicate **value/2**.

### Conclusions

We managed to create three different AIs, including one that was considered to be an 'extra' to this project due to its difficulty (Minimax AI). We also managed to implement all the required functionalities and needed game modes, allowing for players to face each and see other AIs battle it off. More than that, our program allows users to experiment and use different board sizes and number of pieces. With all that said, we do consider this project to be a successful one.

The biggest flaw of the project may be the lacking evaluation method. It is rather simple, due to our non-existent previous knowledge on the game. If we were to improve this project, we would start by investigating more about Yote and coming up with a better solution for the predicate **value/2**.



### Bibliography
https://boardgamegeek.com/boardgame/9385/yote
