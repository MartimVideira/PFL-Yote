# Yote 

- [Yote](https://www.di.fc.ul.pt/~jpn/gv/yote.htm)

The game is played on a 5×6 board, which is empty at the beginning of the game.
Each player has twelve pieces in hand. Players alternate turns, with White
moving first. In a move, a player may either:

- [x] Place a piece in hand on any empty cell of the board.
- [x] Move one of their pieces already on the board orthogonally to an empty adjacent cell.
- [ ] Capture an opponent's piece if it is orthogonally adjacent to a player's piece, by jumping to the empty cell immediately beyond it. The captured piece is removed from the board, and the capturing player removes another of the opponent's pieces of his choosing from the board.
- [ ] The player who captures all the opponent's pieces is the winner. The game can end in a draw if both players are left with three or fewer pieces.

## Optional rules
Yoté is sometimes played using one or more additional rules:

- [ ] Captures are never mandatory.
- [ ] Multiple successive jumps by a piece in a single turn are permitted. After a multi-jump, the player chooses and removes from the board one piece of the opponent for each piece jumped.
- [ ] Capturing entitles the capturing player to a bonus turn.
- [ ] A player can jump one of their own pieces; the piece jumped remains in play.
- [ ] If both players are left with three or fewer pieces, the game immediately ends in a draw.
- [ ] If a player to move has no move available, the game ends and the player with the greater number of pieces remaining is the winner.

### Notes
Game Main Loop Idea
```prolog
repeat.
repeat(X):- repeat.
```
