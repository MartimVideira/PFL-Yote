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

printRound([Board,_Player,[Player1Pieces,Player1Captured],[Player2Pieces,Player2Captured]]):-
    % Possibly Initialize Pieces
    piece(player1,Player1Piece),
    piece(player2,Player2Piece),
    write('Player1'),nl,
    format('Hand ~d ->',[Player1Pieces]),printN(Player1Pieces,Player1Piece),nl,
    format('Captured ~d ->',[Player2Captured]),printN(Player2Captured,Player2Piece),nl,
    printBoard(Board),nl,
    write('Player2'),nl,
    format('Hand ~d ->',[Player2Pieces]),printN(Player2Pieces,Player2Piece),nl,
    format('Captured ~d ->',[Player1Captured]),printN(Player1Captured,Player1Piece),nl,nl.

