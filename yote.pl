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

initialState(State):- 
    piece(emptyCell,EmptyCell), 
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,State).

getCell(State,Column,Line,Cell):-
    notationToInts([Column,Line],[ColumnNumber,LineNumber]),
    at(LineNumber,State,BoardLine),
    at(ColumnNumber,BoardLine,Cell).

getCellInts(State,Column,Line,Cell):-
    at(Line,State,BoardLine),
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

printRound(State):-
    % Possibly Initialize Pieces
    piece(player1,Player1Piece),
    piecesInHand(player1,Player1Pieces),
    piece(player2,Player2Piece),
    piecesInHand(player2,Player2Pieces),
    write('Player1\'s Hand'),nl,
    write('->'),printN(Player1Pieces,Player1Piece),nl,
    printBoard(State),nl,
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
isValidMove(State,_Player,[C,L]):-
    validPosition(C,L),
    piece(emptyCell,EmptyCell),
    getCell(State,C,L,EmptyCell),!.

% Moving A Pice Inside The Board
isValidMove(State,Player,[Ci,Li,Cf,Lf]):-
    validPosition(Ci,Li),
    validPosition(Cf,Lf),
    piece(Player,PlayerPiece),
    getCell(State,Ci,Li,PlayerPiece),
    piece(emptyCell,EmptyCell),
    getCell(State,Cf,Lf,EmptyCell),
    notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]),
    (verticalMove(CCi,LCi,CCf,LCf);
     horizontalMove(CCi,LCi,CCf,LCf);
     captureHorizontal(State,Player, CCi,LCi,CCf,LCf); 
     captureVertical(State,Player, CCi,LCi,CCf,LCf)).

captureHorizontal(State, Player, Ci, Li, Cf, Li):-
    (Ci is Cf - 2, piece(Player,PlayerPiece), getCellInts(State,Cf-1,Li,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ');
    (Ci is Cf + 2, piece(Player,PlayerPiece), getCellInts(State,Cf+1,Li,SelectedCell),  PlayerPiece \= SelectedCell, SelectedCell \= ' ').

captureVertical(State, Player, Ci, Li, Ci, Lf):-
    (Li is Lf - 2, piece(Player,PlayerPiece), getCellInts(State,Ci,Lf - 1,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ');
    (Li is Lf + 2, piece(Player,PlayerPiece), getCellInts(State,Ci,Lf + 1,SelectedCell), PlayerPiece \= SelectedCell, SelectedCell \= ' ').


verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf + 1.
verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf - 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf + 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf - 1.

whyNotValid(_State,_Player,[C,L]):-
    \+ validPosition(C,L),!,
    write('That Is Not A Valid Position\n').
whyNotValid(State,_Player,[C,L]):-
    getCell(State,C,L,Cell),
    write('Cell In This Position Is:'),write(Cell),nl.
whyNotValid(_State,_Player,[Ci,Li,_Cf,_Lf]):-
    \+validPosition(Ci,Li),
    write('That Is Not A Valid Position\n').
whyNotValid(_State,_Player,[_Ci,_Li,Cf,Lf]):-
    \+validPosition(Cf,Lf),
    write('That Is Not A Valid Position\n').
whyNotValid(_State,_Player,Move):-
    notationToInts(Move,[Ci,Li,Cf,Lf]),
    \+ (verticalMove(Ci,Li,Cf,Lf);
        horizontalMove(Ci,Li,Cf,Lf);
        captureHorizontal(_State, _Player, Ci,Li,Cf,Lf); 
        captureVertical(_State, _Player, Ci,Li,Cf,Lf)),
    write('That Move Is Not Orthogonal\n').
whyNotValid(State,Player,[Ci,Li,_Cf,_Lf]):-
    piece(Player,PlayerPiece),
    getCell(State,Ci,Li,SelectedCell),
    PlayerPiece \= SelectedCell,
    write('Selected Cell Is Not '),write(Player),write('\'s Piece\n').

validatePlayerMove(State,Player,Move):-
    isValidMove(State,Player,Move),!.
validatePlayerMove(State,Player,Move):-
    write('[INVALID MOVE] '),
    whyNotValid(State,Player,Move),fail.

getPlayerMove(State,Player,Move):-
    write(Player),write(' Next Move:'),
    read(AtomMove),
    atom_chars(AtomMove,Move),
    validatePlayerMove(State,Player,Move),!.
getPlayerMove(State,Player,Move):- getPlayerMove(State,Player,Move).

playMove(State,Player,[C,L],NewState):-
    decrement_hand_pieces(Player),
    at(L,State,Line),
    piece(Player,PlayerPiece),
    setAt(C,Line,PlayerPiece,NewLine),
    setAt(L,State,NewLine,NewState).
playMove(State,Player,[Ci,Li,Cf,Lf],NewState):-
    at(Li,State,Line),
    piece(Player,PlayerPiece),
    piece(emptyCell,EmptyCell),
    setAt(Ci,Line,EmptyCell,OldLine),
    setAt(Li,State,OldLine,PartialState),
    at(Lf,PartialState,FinalLine),
    setAt(Cf,FinalLine,PlayerPiece,NewFinalLine),
    setAt(Lf,PartialState,NewFinalLine,FinalState),
    removePieces(Player, FinalState, [Ci, Li, Cf, Lf], NewState).


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

playRound(State,Player):-

    printRound(State),
    getPlayerMove(State,Player,Move),
    notationToInts(Move,ConvertedMove),
    playMove(State,Player,ConvertedMove,NewState),
    nextPlayer(Player,NextPlayer),
    playRound(NewState,NextPlayer).


increment_captured_pieces(Player):-
    retract(piecesCaptured(Player, PlayerPieces)),
    assertz(piecesCaptured(Player, PlayerPieces + 1)).
    
decrement_hand_pieces(Player):-
    retract(piecesInHand(Player, PlayerPieces)),
    assertz(piecesInHand(Player, PlayerPieces - 1)).
