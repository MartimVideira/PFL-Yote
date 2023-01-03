
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

/**
 * printHeader
 *
 * Prints game header.
 */
printHeader:-
    write('   '),
    numberColumns(Columns),
    myTake(Columns,"ABCDEFGHIJKLMNOPQRSTUVXYZ",Letras),
    myIntersperse(Letras," ",Header),
    printList(Header),nl.

/**
 * printBoard(+ Board)
 *
 * Prints game board.
 */
printBoard(Board):-
    numberLines(LineNumber),
    printHeader,
    printLines(Board,LineNumber).

printLines([],_).
printLines([Line|Board],RowNumber):-
    printLine(Line,RowNumber),
    RowsLeft is RowNumber -1,
    printLines(Board,RowsLeft).



/**
 * displayGame(+ [Board,_Player,[Player1Pieces,Player1Captured],[Player2Pieces,Player2Captured]])
 *
 * It receives a variable that represents the Board State, with information on the pieces of each player,
 * including the ones that were captured. After that, prints out the game for the user to play via
 * the console / interface.
 */
display_game([Board,_Player,[Player1Pieces,Player1Captured],[Player2Pieces,Player2Captured]]):-
    % Possibly Initialize Pieces
    write('\33\[2J'),
    piece(player1,Player1Piece),
    piece(player2,Player2Piece),
    write('Player1'),nl,
    format('Hand ~d ->',[Player1Pieces]),printN(Player1Pieces,Player1Piece),nl,
    format('Captured ~d ->',[Player2Captured]),printN(Player2Captured,Player2Piece),nl,
    printBoard(Board),nl,
    write('Player2'),nl,
    format('Hand ~d ->',[Player2Pieces]),printN(Player2Pieces,Player2Piece),nl,
    format('Captured ~d ->',[Player1Captured]),printN(Player1Captured,Player1Piece),nl,nl.

