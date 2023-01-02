% Game Variables For More Flexibility
numberColumns(6).
numberLines(5).
numberPieces(2).
piece(emptyCell,' ').
piece(player1,'O').
piece(player2,'X').

% Nice Rule
nextPlayer(player1,player2).
nextPlayer(player2,player1).

% Nice Api To interact With The Game State
getPlayer([_,Player|_],Player).
setPlayer([Board,_|Rest],Player,[Board,Player|Rest]).
getBoard([Board|_],Board).
setBoard([_|Rest],Board,[Board|Rest]).
getPlayerPieces([_,player1,Pieces,_],Pieces).
getPlayerPieces([_,player2,_,Pieces],Pieces).
setPlayerPieces([Board,player1,_,Pieces2],Pieces,[Board,player1,Pieces,Pieces2]).
setPlayerPieces([Board,player2,Pieces1,_],Pieces,[Board,player2,Pieces1,Pieces]).

getCell(Board,Column,Line,Cell):-
    at(Line,Board,BoardLine),
    at(Column,BoardLine,Cell).

increment_captured_pieces([Board,Player|Rest],NewState):-
    nextPlayer(Player,NextPlayer),
    getPlayerPieces([Board,NextPlayer|Rest],[InHand,Captured]),
    NewCaptured is Captured + 1,
    setPlayerPieces([Board,NextPlayer|Rest],[InHand,NewCaptured],PartialState),
    setPlayer(PartialState,Player,NewState).

decrement_hand_pieces(State,NewState):-
    getPlayerPieces(State,[InHand,Captured]),
    NewInHand is InHand - 1,
    setPlayerPieces(State,[NewInHand,Captured],NewState).

checkWinCondition([Board,_|Rest],player2):-
    numberPieces(N),
    getPlayerPieces([Board,player2|Rest],[_,N]).
    
checkWinCondition([Board,_|Rest],player1):-
    numberPieces(N),
    getPlayerPieces([Board,player1|Rest],[_,N]).

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

removeCapturedPiece([Board|Rest], C, L,NewState):-
    at(L,Board,Line),
    piece(emptyCell,EmptyCell),
    setAt(C, Line, EmptyCell ,OldLine),
    setAt(L,Board,OldLine,NewBoard),
    increment_captured_pieces([NewBoard|Rest],NewState).

initialState([Board,player1,[N,0],[N,0]]):- 
    numberPieces(N),
    piece(emptyCell,EmptyCell), 
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,Board).



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