?- set_prolog_flag(double_quotes,chars).

% Game Variables/Rules
numberColumns(6).
numberLines(5).
emptyCell(' ').
piece(player1,'O').
piece(player2,'X').

piecesInHand(player1,12).
piecesInHand(player2,12).
piecesInPlay(player1,0).
piecesInPlay(player2,0).

initialState(State):- 
    emptyCell(EmptyCell),
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,State).


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
    L >= 1,
    L =< 5,
    char_code(C,CCode),
    char_code('f',FCode),
    char_code('a',ACode),
    CCode =< FCode,
    CCode >= ACode.

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
