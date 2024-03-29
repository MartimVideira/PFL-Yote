:- use_module(library(clpfd)).


/**
 * notationToInts(+[Column,Line], -[ColumnNumber,LineNumber]])
 *
 * Method that receives a char representing a column of the board and an integer representing
 * a line of the board where they want to play the piece, and converts them into
 * a list of two integers, the number of the column and number of the line, respectively.
 * For example, it receives ["a", 5] and turns it into [0, 5].
 */
notationToInts([Column,Line],[ColumnNumber,LineNumber]):-
    
    char_code('a',ACode),
    char_code('1',OneCode),
    numberLines(TotalLines),
    TotalLines1 is TotalLines - 1,
    ColumnNumber #= ColumnCode - ACode,
    LineNumber #= TotalLines1 - (LineCode - OneCode),
    char_code(Column,ColumnCode),
    char_code(Line,LineCode),
    !.

/**
 * notationToInts(+ [Ci,Li,Cf,Lf], - [CCi,LCi,CCf,LCf])
 *
 * Method that receives the notation of a move they want to play to move a piece, and converts them into
 * a list of four integers, the numbers of the columns and numbers of the lines, respectively.
 * For example, it receives ["a", 5, "a", 6] and turns it into [0, 5, 0, 6].
 */
notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]):-
    notationToInts([Ci,Li],[CCi,LCi]),
    notationToInts([Cf,Lf],[CCf,LCf]),!.


/**
 * validatePlayerMove(+ State, + Move)
 *
 * Wrapper that checks wheter a certain move is valid or not by calling isValidMove(+ State, + Move),
 * and in the case it is not, displays the reason why by invoking whyNotValid( + State, + Move) method.
 */
validatePlayerMove(State,Move):-
    isValidMove(State,Move),!.
validatePlayerMove(State,Move):-
    write('[INVALID MOVE] '),
    whyNotValid(State,Move),fail.


/**
 * getPlayerMove(+ [Board,Player|Rest], - Move)
 *
 * Retrieves the player's move of choice from the interface by reading it.
 */
getPlayerMove([Board,Player|Rest],Move):-
    write(Player),write(' Next Move:'),
    read(AtomMove),
    atom_chars(AtomMove,Move),
    validatePlayerMove([Board,Player|Rest],Move),!.
getPlayerMove(State,Move):- getPlayerMove(State,Move).


/**
 * validPosition(+ C, + L)
 *
 * Checks whether or not a position is valid for the player to use it.
 */
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

/**
 * verticalMove(+ Ci,+ Li,+ Ci,+ Lf)
 *
 * Checks whether or not the move made was done vertically
 */
verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf + 1.
verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf - 1.

/**
 * horizontalMove(+ Ci,+ Li,+ Cf,+ Li)
 *
 * Checks whether or not the move made was done horizontally
 */
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf + 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf - 1.

/**
 * captureHorizontal(+ Board, + Player, + Ci,+ Li,+ Cf,+ Li)
 *
 * Checks if the movement made was a horizontal capture.
 */
captureHorizontal(Board,Player,Ci, Li, Cf, Li):-
    (
        piece(emptyCell,EmptyCell),
        piece(Player,PlayerPiece),
        Ci is Cf - 2, 
        getCell(Board,Cf-1,Li,SelectedCell),
        PlayerPiece \= SelectedCell,
        SelectedCell \= EmptyCell
    );

    (
        piece(emptyCell,EmptyCell),
        piece(Player,PlayerPiece),
        Ci is Cf + 2, 
        getCell(Board,Cf+1,Li,SelectedCell), 
        PlayerPiece \= SelectedCell, 
        SelectedCell \= EmptyCell
    ).

/**
 * captureVertical(+ Board, + Player, + Ci,+ Li,+ Ci,+ Lf)
 *
 * Checks if the movement made was a vertical capture.
 */
captureVertical(Board,Player,Ci, Li, Ci, Lf):-
    (
        piece(Player,PlayerPiece),
        piece(emptyCell,EmptyCell),
        Li is Lf - 2,  
        getCell(Board,Ci,Lf - 1,SelectedCell),
        PlayerPiece \= SelectedCell, SelectedCell\= EmptyCell
    );
    (
        piece(Player,PlayerPiece),
        piece(emptyCell,EmptyCell),
        Li is Lf + 2,
        getCell(Board,Ci,Lf + 1,SelectedCell),
        PlayerPiece \= SelectedCell, SelectedCell\= EmptyCell
    ).

/**
 * isValidMove(State,[+ C, + L])
 *
 * Checks if placing a piece is valid or not.
 */
isValidMove(State,[C,L]):-
    getPlayerPieces(State,[PiecesInHand,_]),
    PiecesInHand > 0,
    getBoard(State,Board),
    validPosition(C,L),
    piece(emptyCell,EmptyCell),
    notationToInts([C,L],[CC,LC]),
    getCell(Board,CC,LC,EmptyCell),!.

/**
 * isValidMove(+ [Board,Player|_], + [Ci,Li,Cf,Lf])
 *
 * Checks if moving a piece is valid or not.
 */
isValidMove([Board,Player|_],[Ci,Li,Cf,Lf]):-
    validPosition(Ci,Li),
    validPosition(Cf,Lf),
    notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]),
    piece(Player,PlayerPiece),
    getCell(Board,CCi,LCi,PlayerPiece),
    piece(emptyCell,EmptyCell),
    getCell(Board,CCf,LCf,EmptyCell),
    (verticalMove(CCi,LCi,CCf,LCf);
     horizontalMove(CCi,LCi,CCf,LCf);
     captureHorizontal(Board,Player, CCi,LCi,CCf,LCf); 
     captureVertical(Board,Player, CCi,LCi,CCf,LCf)).

/**
 * whyNotValid(+ _State, +[C,L])
 *
 * Displays and prints to screen why move what not valid.
 */
whyNotValid(_State,[C,L]):-
    \+ validPosition(C,L),!,
    write('That Is Not A Valid Position\n').
whyNotValid(State,[_C,_L]):-
    getPlayerPieces(State,[PiecesInHand,_]),
    PiecesInHand =< 0,
    write('You dont\'t have any more pieces in hand').
whyNotValid([Board|_],[C,L]):-
    notationToInts([C,L],[CC,LC]),
    getCell(Board,CC,LC,Cell),
    write('Cell In This Position Is:'),write(Cell),nl,!.
whyNotValid(_State,[Ci,Li,Cf,Lf]):-
    (\+validPosition(Ci,Li);
    \+validPosition(Cf,Lf)),
    write('That Is Not A Valid Position\n'),!.
whyNotValid([Board,Player|_],Move):-
    notationToInts(Move,[Ci,Li,Cf,Lf]),
    \+ (verticalMove(Ci,Li,Cf,Lf);
        horizontalMove(Ci,Li,Cf,Lf);
        captureHorizontal(Board, Player, Ci,Li,Cf,Lf); 
        captureVertical(Board, Player, Ci,Li,Cf,Lf)),
    write('That Move Is Not Orthogonal\n'),!.
whyNotValid([Board,Player|_],Move):-
    piece(Player,PlayerPiece),
    notationToInts(Move,[Ci,Li|_]),
    getCell(Board,Ci,Li,SelectedCell),
    PlayerPiece \= SelectedCell,
    write('Selected Cell Is Not '),write(Player),write('\'s Piece\n'),!.
whyNotValid([Board|_],Move):-
    piece(emptyCell,EmptyCell),
    notationToInts(Move,[_,_,Cf,Lf]),
    getCell(Board,Cf,Lf,SelectedCell),
    SelectedCell \= EmptyCell,
    write('Moving Into Another Players Piece!\n'),!.
whyNotValid(_,_):-
    write('This Reason Is Not Being Checked').

