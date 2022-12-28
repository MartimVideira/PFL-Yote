?- set_prolog_flag(double_quotes,chars).

% Game Variables/Rules
numberColumns(6).
numberLines(5).
piece(emptyCell,' ').
piece(player1,'O').
piece(player2,'X').

piecesInHand(player1,12).
piecesInHand(player2,12).
piecesInPlay(player1,0).
piecesInPlay(player2,0).

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
validPosition(C,L):-
    write(C),write(L),write(' Not A Valid Position'),nl.

% Moving A Piece Into The Board
isValidMove(State,_Player,[C,L]):-
    validPosition(C,L),
    piece(emptyCell,EmptyCell),
    getCell(State,C,L,EmptyCell),!.
isValidMove(State,_Player,[C,L]):-
    validPosition(C,L),
    getCell(State,C,L,Cell),
    write('Cell In This Position Is:'),write(Cell),nl.
% Moving A Pice Inside The Board
isValidMove(State,Player,[Ci,Li,Cf,Lf]):-
    validPosition(Ci,Li),
    validPosition(Cf,Lf),
    piece(Player,PlayerPiece),
    getCell(State,Ci,Li,PlayerPiece),
    piece(emptyCell,EmptyCell),
    getCell(State,Cf,Lf,EmptyCell).

getMove(State,Player,Move):-
    write(Player),write(' Next Move:'),
    read(Move),
    write(Move),
    isValidMove(State,Player,Move),!.
getMove(State,Player,Move):- getMove(State,Player,Move).

playMove(State,Player,[C,L],NewState):-
    notationToInts([C,L],[ColumnNumber,LineNumber]),
    at(LineNumber,State,Line),
    piece(Player,PlayerPiece),
    setAt(ColumnNumber,Line,PlayerPiece,NewLine),
    setAt(LineNumber,State,NewLine,NewState).

playRound(State,Player):-
    printRound(State),
    getMove(State,Player,Move),
    write(Move),
    playMove(State,Player,Move,NewState),
    playRound(NewState,Player).

% Helper Functions

% Creates in XS a list of N Xs.
myRepeat(_,0,[]):-!.
myRepeat(X,N,[X|XS]):- 
    N1 is N - 1,
    myRepeat(X,N1,XS).

printList([]).
printList([X|XS]):- write(X),printList(XS).

printN(0,_):-!.
printN(N,X):- write(X), N1 is N - 1, printN(N1,X).

myConcat([],YS,YS).
myConcat([X|XS],YS,[X|ZS]):- myConcat(XS,YS,ZS).

myIntersperse([],_,[]).
myIntersperse([X|XS],YS,Res):-
    ThisRes = [X | YS],
    myIntersperse(XS,YS,Res1),
    myConcat(ThisRes,Res1,Res).

at(0,[X|_],X):-!.
at(N,[_|XS],Elem):- N1 is N -1 , at(N1,XS,Elem).

setAt(0,[_|XS],Y,[Y|XS]):-!.
setAt(N,[X|XS],Y,[X|ZS]):-
    N1 is N - 1,
    setAt(N1,XS,Y,ZS).
