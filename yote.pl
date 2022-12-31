?- set_prolog_flag(double_quotes,chars).
:- consult('utils.pl').
:- dynamic(piecesInHand/2).
:- dynamic(piecesCaptured/2).

% Game Variables/Rules
numberColumns(6).
numberLines(5).
piece(emptyCell,' ').
piece(player1,'O').
piece(player2,'X').

piecesInHand(player1,12).
piecesInHand(player2,12).
piecesCaptured(player1,0).
piecesCaptured(player2,0).
piecesInPlay(player1,0).
piecesInPlay(player2,0).

nextPlayer(player1,player2).
nextPlayer(player2,player1).

notationToInts([Column,Line],[ColumnNumber,LineNumber]):-
    char_code(Column,ColumnCode),
    char_code('a',ACode),
    char_code('1',OneCode),
    char_code(Line,LineCode),
    ColumnNumber is ColumnCode - ACode,
    LineNumber is 4 - (LineCode - OneCode),!.
notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]):-
    notationToInts([Ci,Li],[CCi,LCi]),
    notationToInts([Cf,Lf],[CCf,LCf]),!.

initialState([Board,player1,[12,0],[12,0]]):- 
    piece(emptyCell,EmptyCell), 
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,Board).

getCell(Board,Column,Line,Cell):-
    notationToInts([Column,Line],[ColumnNumber,LineNumber]),
    at(LineNumber,Board,BoardLine),
    at(ColumnNumber,BoardLine,Cell).

getCellInts([Board|_Rest],Column,Line,Cell):-
    at(Line,Board,BoardLine),
    at(Column,BoardLine,Cell).

% Board Printing Rules
printLine([]):-
    write('|'),nl.
printLine([Cell|Rest]):-
    write('|'),
    write(Cell),
    printLine(Rest).
printLine(Line,Number):-
    write(Number),
    write(' '),
    printLine(Line).

printHeader:-
    write('   '),
    myIntersperse("ABCDEF"," ",Header),
    printList(Header),nl.

printBoard(Board):-
    numberLines(LineNumber),
    printHeader,
    printLines(Board,LineNumber).

printLines([],_).
printLines([Line|Board],RowNumber):-
    printLine(Line,RowNumber),
    RowsLeft is RowNumber -1,
    printLines(Board,RowsLeft).

printRound([Board,_Player,[Player1Pieces,_],[Player2Pieces,_]]):-
    % Possibly Initialize Pieces
    piece(player1,Player1Piece),
    piece(player2,Player2Piece),
    write('Player1\'s Hand'),nl,
    write('->'),printN(Player1Pieces,Player1Piece),nl,
    printBoard(Board),nl,
    write('Player2\'s Hand'),nl,
    write('->'),printN(Player2Pieces,Player2Piece),nl,nl.

validPosition(C,L):-
    char_code(L,LCode),
    char_code('1',OneCode),
    char_code('5',FiveCode),
    LCode >= OneCode,
    LCode =< FiveCode,
    char_code(C,CCode),
    char_code('f',FCode),
    char_code('a',ACode),
    CCode =< FCode,
    CCode >= ACode,!.

% Moving A Piece Into The Board
isValidMove([Board|_],[C,L]):-
    validPosition(C,L),
    piece(emptyCell,EmptyCell),
    getCell(Board,C,L,EmptyCell),!.

% Moving A Pice Inside The Board
isValidMove([Board,Player|_],[Ci,Li,Cf,Lf]):-
    validPosition(Ci,Li),
    validPosition(Cf,Lf),
    piece(Player,PlayerPiece),
    getCell(Board,Ci,Li,PlayerPiece),
    piece(emptyCell,EmptyCell),
    getCell(Board,Cf,Lf,EmptyCell),
    notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]),
    (verticalMove(CCi,LCi,CCf,LCf);
     horizontalMove(CCi,LCi,CCf,LCf);
     captureHorizontal(Board,Player, CCi,LCi,CCf,LCf); 
     captureVertical(Board,Player, CCi,LCi,CCf,LCf)).

captureHorizontal([Board,Player|_],Ci, Li, Cf, Li):-
    (Ci is Cf - 2, piece(Player,PlayerPiece), getCellInts(Board,Cf-1,Li,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ');
    (Ci is Cf + 2, piece(Player,PlayerPiece), getCellInts(Board,Cf+1,Li,SelectedCell),  PlayerPiece \= SelectedCell, SelectedCell \= ' ').

captureVertical([Board,Player|_],Ci, Li, Ci, Lf):-
    (Li is Lf - 2, piece(Player,PlayerPiece), getCellInts(Board,Ci,Lf - 1,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ');
    (Li is Lf + 2, piece(Player,PlayerPiece), getCellInts(Board,Ci,Lf + 1,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ').


verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf + 1.
verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf - 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf + 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf - 1.

whyNotValid(_State,[C,L]):-
    \+ validPosition(C,L),!,
    write('That Is Not A Valid Position\n').
whyNotValid([Board|_],[C,L]):-
    getCell(Board,C,L,Cell),
    write('Cell In This Position Is:'),write(Cell),nl.
whyNotValid(_State,[Ci,Li,Cf,Lf]):-
    (\+validPosition(Ci,Li);
    \+validPosition(Cf,Lf)),
    write('That Is Not A Valid Position\n').
whyNotValid([Board,Player|_],Move):-
    notationToInts(Move,[Ci,Li,Cf,Lf]),
    \+ (verticalMove(Ci,Li,Cf,Lf);
        horizontalMove(Ci,Li,Cf,Lf);
        captureHorizontal(Board, Player, Ci,Li,Cf,Lf); 
        captureVertical(Board, Player, Ci,Li,Cf,Lf)),
    write('That Move Is Not Orthogonal\n').
whyNotValid([Board,Player|_],[Ci,Li,_Cf,_Lf]):-
    piece(Player,PlayerPiece),
    getCell(Board,Ci,Li,SelectedCell),
    PlayerPiece \= SelectedCell,
    write('Selected Cell Is Not '),write(Player),write('\'s Piece\n').

validatePlayerMove(State,Move):-
    isValidMove(State,Move),!.
validatePlayerMove(State,Move):-
    write('[INVALID MOVE] '),
    whyNotValid(State,Move),fail.

getPlayerMove([Board,Player|Rest],Move):-
    write(Player),write(' Next Move:'),
    read(AtomMove),
    atom_chars(AtomMove,Move),
    validatePlayerMove([Board,Player|Rest],Move),!.
getPlayerMove(State,Move):- getPlayerMove(State,Move).

playMove([Board,Player|Rest],[C,L],NewState):-
    at(L,Board,Line),
    piece(Player,PlayerPiece),
    setAt(C,Line,PlayerPiece,NewLine),
    setAt(L,Board,NewLine,NewBoard),
    decrement_hand_pieces([NewBoard,Player|Rest],Player,NewState).
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


% Refazer esta funcao
removePieces(Player, State,[Ci,Li,Cf,Lf], NewState):-
    (((Ci is Cf - 2, removeCapturedPiece(State, Cf - 1, Li, NewState));
    (Ci is Cf + 2, removeCapturedPiece(State, Cf + 1, Li, NewState));
    (Li is Lf - 2, removeCapturedPiece(State, Cf, Lf - 1, NewState));
    (Li is Lf + 2, removeCapturedPiece(State, Cf, Lf + 1, NewState))), increment_captured_pieces(Player));

    ( at(Li,State,Line),
    setAt(Li, State,Line,NewState),
    true).

removeCapturedPiece(State, C, L, NewState):-

    at(L,State,Line),
    piece(emptyCell,EmptyCell),
    setAt(C, Line, EmptyCell ,OldLine),
    setAt(L, State,OldLine,NewState).

playRound(State):-
    printRound(State),
    getPlayerMove(State,Move),
    notationToInts(Move,ConvertedMove),
    playMove(State,ConvertedMove,[NewBoard,Player|Rest]),
    nextPlayer(Player,NextPlayer),
    playRound([NewBoard,NextPlayer|Rest]).

% Refazer esta funcão
increment_captured_pieces(Player):-
    retract(piecesCaptured(Player, PlayerPieces)),
    assertz(piecesCaptured(Player, PlayerPieces + 1)).
    
% Refazer esta funcao
decrement_hand_pieces(Player):-
    retract(piecesInHand(Player, PlayerPieces)),
    assertz(piecesInHand(Player, PlayerPieces - 1)).
