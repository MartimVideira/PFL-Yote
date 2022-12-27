?- set_prolog_flag(double_quotes,chars).
% Creates in XS a list of N Xs.
myRepeat(_,0,[]):-!.
myRepeat(X,N,[X|XS]):- 
    N1 is N - 1,
    myRepeat(X,N1,XS).

emptyCell(' ').
player1Piece('O').
player2Piece('X').
numberColumns(6).
numberLines(5).

initialState(State):- 
    emptyCell(EmptyCell),
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,State).


printN(0,_):-!.
printN(N,X):- write(X), N1 is N - 1, printN(N1,X).

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

myConcat([],YS,YS).
myConcat([X|XS],YS,[X|ZS]):- myConcat(XS,YS,ZS).

myIntersperse([],_,[]).
myIntersperse([X|XS],YS,Res):-
    ThisRes = [X | YS],
    myIntersperse(XS,YS,Res1),
    myConcat(ThisRes,Res1,Res).

printList([]).
printList([X|XS]):- write(X),printList(XS).
printHeader:-
    write('   '),
    myIntersperse("ABCDEF"," ",Header),
    printList(Header),nl.

printBoard(Board):-
    numberLines(LineNumber),
    printHeader,
    printLines(Board,LineNumber).

printLines([Line|Board],RowNumber):-
    printLine(Line,RowNumber),
    RowsLeft is RowNumber -1,
    printLines(Board,RowsLeft).
