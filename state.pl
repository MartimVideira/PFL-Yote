% Game Variables For More Flexibility
:- dynamic numberColumns/1.
:- dynamic numberLines/1.
:- dynamic numberPieces/1.
numberColumns(6).
numberLines(5).
numberPieces(12).
piece(emptyCell,' ').
piece(player1,'O').
piece(player2,'X').

%Gives next player
nextPlayer(player1,player2).
nextPlayer(player2,player1).

% API's To interact With The Game State
getPlayer([_,Player|_],Player).
setPlayer([Board,_|Rest],Player,[Board,Player|Rest]).
getBoard([Board|_],Board).
setBoard([_|Rest],Board,[Board|Rest]).
getPlayerPieces([_,player1,Pieces,_],Pieces).
getPlayerPieces([_,player2,_,Pieces],Pieces).
setPlayerPieces([Board,player1,_,Pieces2],Pieces,[Board,player1,Pieces,Pieces2]).
setPlayerPieces([Board,player2,Pieces1,_],Pieces,[Board,player2,Pieces1,Pieces]).


/**
 * getCell(+ Board,+ Column,+ Line,- Cell)
 *
 * Retrieves Cell present in given board in the position specified by Line and Column.
 */
getCell(Board,Column,Line,Cell):-
    at(Line,Board,BoardLine),
    at(Column,BoardLine,Cell).


/**
 * increment_captured_pieces(+ [Board,Player|Rest], - NewState)
 *
 * Changes the current state of the board and players' pieces by incrementing the number of
 * captured pieces of a player. Then, returns the new board state and knowledge on the argument - NewState.
 */
increment_captured_pieces([Board,Player|Rest],NewState):-
    nextPlayer(Player,NextPlayer),
    getPlayerPieces([Board,NextPlayer|Rest],[InHand,Captured]),
    NewCaptured is Captured + 1,
    setPlayerPieces([Board,NextPlayer|Rest],[InHand,NewCaptured],PartialState),
    setPlayer(PartialState,Player,NewState).


/**
 * increment_captured_pieces(+ State, - NewState)
 *
 * Changes the current state of the board and players' pieces by decrementing the number of
 * pieces in hand of a player. Then, returns the new board state and knowledge on the argument - NewState.
 */
decrement_hand_pieces(State,NewState):-
    getPlayerPieces(State,[InHand,Captured]),
    NewInHand is InHand - 1,
    setPlayerPieces(State,[NewInHand,Captured],NewState).

/**
 * game_over(+ [Board,_|Rest], + player2)
 *
 * Sets the condition for when the should end, declaring the winner.
 */
game_over([Board,_|Rest],player2):-
    numberPieces(N),
    getPlayerPieces([Board,player2|Rest],[_,N]).
    
game_over([Board,_|Rest],player1):-
    numberPieces(N),
    getPlayerPieces([Board,player1|Rest],[_,N]).


/**
 * removePieces( + State, + [Ci,Li,Cf,_Lf], - NewState)
 *
 * Checks whether or not there are pieces to be removed, if so, calls
 * method removeCapturedPieces(+ State, + C, + L, - NewState) to place empty cells in their spot.
 */
removePieces(State,[Ci,Li,Cf,_Lf], NewState):-
    Ci is Cf - 2,
    removeCapturedPiece(State, Cf - 1, Li,NewState).
removePieces(State,[Ci,Li,Cf,_Lf], NewState):-
    Ci is Cf + 2,
    removeCapturedPiece(State, Cf + 1, Li,NewState).
removePieces(State,[_Ci,Li,Cf,Lf], NewState):-
    Li is Lf - 2,
    removeCapturedPiece(State, Cf, Lf - 1,NewState).
removePieces(State,[_Ci,Li,Cf,Lf], NewState):-
    Li is Lf + 2,
    removeCapturedPiece(State, Cf, Lf + 1,NewState).
removePieces(State,_,State).


/**
 * removeCapturedPieces(+ State, + C, + L, - NewState)
 *
 * Places empty cells on the place of previously captured pieces.
 */
removeCapturedPiece([Board|Rest], C, L,NewState):-
    at(L,Board,Line),
    piece(emptyCell,EmptyCell),
    setAt(C, Line, EmptyCell ,OldLine),
    setAt(L,Board,OldLine,NewBoard),
    increment_captured_pieces([NewBoard|Rest],NewState).


/**
 * initial_state(+ [Board,player1,[N,0],[N,0]])
 *
 * Receives a board with no players' pieces and turns it into its initial state.
 */
initial_state([Board,player1,[N,0],[N,0]]):- 
    numberPieces(N),
    piece(emptyCell,EmptyCell), 
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,Board).


/**
 * playMove(+ [Board,Player|Rest],+[C,L], - NewState)
 *
 * Receives a Board, places the piece at column C, Line L 
 * and returns the new board via the argument - NewState.
 */
playMove([Board,Player|Rest],[C,L],NewState):-
    at(L,Board,Line),
    piece(Player,PlayerPiece),
    setAt(C,Line,PlayerPiece,NewLine),
    setAt(L,Board,NewLine,NewBoard),
    decrement_hand_pieces([NewBoard,Player|Rest],NewState).

playMove([Board,Player|Rest],[Ci,Li,Cf,Lf],FinalState):-
    at(Li,Board,Line),
    piece(Player,PlayerPiece),
    piece(emptyCell,EmptyCell),
    setAt(Ci,Line,EmptyCell,OldLine),
    setAt(Li,Board,OldLine,PartialBoard),
    at(Lf,PartialBoard,FinalLine),
    setAt(Cf,FinalLine,PlayerPiece,NewFinalLine),
    setAt(Lf,PartialBoard,NewFinalLine,FinalBoard),
    removePieces([FinalBoard,Player|Rest], [Ci, Li, Cf, Lf],FinalState).