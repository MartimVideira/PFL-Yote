?- set_prolog_flag(double_quotes,chars).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('utils.pl').
:- use_module(library(clpfd)).


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

notationToInts([Column,Line],[ColumnNumber,LineNumber]):-
    
    char_code('a',ACode),
    char_code('1',OneCode),

    ColumnNumber #= ColumnCode - ACode,
    LineNumber #= 4 - (LineCode - OneCode),
    char_code(Column,ColumnCode),
    char_code(Line,LineCode),
    !.

notationToInts([Ci,Li,Cf,Lf],[CCi,LCi,CCf,LCf]):-
    notationToInts([Ci,Li],[CCi,LCi]),
    notationToInts([Cf,Lf],[CCf,LCf]),!.

initialState([Board,player1,[N,0],[N,0]]):- 
    numberPieces(N),
    piece(emptyCell,EmptyCell), 
    numberColumns(NumberColumns),
    numberLines(NumberLines),
    myRepeat(EmptyCell,NumberColumns,Columns),
    myRepeat(Columns,NumberLines,Board).


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
isValidMove(State,[C,L]):-
    getPlayerPieces(State,[PiecesInHand,_]),
    PiecesInHand > 0,
    getBoard(State,Board),
    validPosition(C,L),
    piece(emptyCell,EmptyCell),
    notationToInts([C,L],[CC,LC]),
    getCell(Board,CC,LC,EmptyCell),!.

% Moving A Pice Inside The Board
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

verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf + 1.
verticalMove(Ci,Li,Ci,Lf):-
    Li is Lf - 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf + 1.
horizontalMove(Ci,Li,Cf,Li):-
    Ci is Cf - 1.

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


playRound(State):-
    checkWinCondition(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').

playRound(State):-
    printRound(State),
    getPlayerMove(State,Move),
    notationToInts(Move,ConvertedMove),
    playMove(State,ConvertedMove,[NewBoard,Player|Rest]),!,
    nextPlayer(Player,NextPlayer),
    playRound([NewBoard,NextPlayer|Rest]),!.


playGame:-
    initialState(S),
    playRound(S).


playDumbAI:-
    initialState(S),
    playRoundDumbAI(S).

playDumbAI(State):-
    checkWinCondition(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').
    
playRoundDumbAI([Board, player2|Rest]):-

    getValidMoves([Board ,player2|Rest], Moves),
    random_member(MoveAI, Moves),
    write(MoveAI),
    playMove([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundDumbAI([NewBoard,player1|NewRest]).

playRoundDumbAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),!,
    write('Played Human Move'),
    playRoundDumbAI([NewBoard,player2|NewRest]).

playRoundHuman([Board, player1|Rest],NewState):-
    printRound([Board, player1|Rest]),
    getPlayerMove([Board, player1|Rest],Move),
    notationToInts(Move,ConvertedMove),
    playMove([Board, player1|Rest],ConvertedMove,NewState).

playAI:-
    initialState(State),
    playRoundAI(State).

playRoundAI(State):-
    checkWinCondition(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').

playRoundAI([Board, player2|Rest]):-
    getValidMoves([Board, player2|Rest], Moves),
    greedyChoice([Board ,player2|Rest], MoveAI, player2, Moves),
    write(MoveAI),
    playMove([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundAI([NewBoard,player1|NewRest]).

playRoundAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),
    playRoundAI([NewBoard,player2|NewRest]).

evaluate_board([Board, player2|Rest], player2, Score):-
    getPlayerPieces([Board,player2|Rest], [P2,N2]),
    getPlayerPieces([Board,player1|Rest], [P1,N1]),
    Score = P2.

evaluate_board([Board, player1|Rest], player1, Score):-
    getPlayerPieces([Board,player2|Rest], [_,N]),
    getPlayerPieces([Board,player1|Rest], [_,M]),
    Score = 10 * M - (10 * N).

getValidMoves(State, Moves):-
    getValidMoves2(State, Moves2),
    getValidMoves1(State, Moves1),
    myConcat(Moves1, Moves2, Moves).

getValidMoves1(State, Moves):- 
    setof([Ci, Li, Cf, Lf], (Notation, State)^
(   between(0, 5, Ci),
    between(0, 4, Li),
    between(0, 5, Cf),
    between(0, 4, Lf),
    notationToInts(Notation, [Ci, Li, Cf, Lf]),
    isValidMove(State, Notation)), Moves) ; Moves = [].
getValidMoves2(State, Moves):- 
        setof([C, L], (Notation, State)^
(   between(0, 5, C),
    between(0, 4, L),
    notationToInts(Notation, [C, L]),
    isValidMove(State, Notation)),
    Moves); Moves =[].

greedyChoice([Board ,player2|Rest], Move, player2, Moves) :-

    setof(Score-Mv, NewGameState^Moves^(
        member(Mv, Moves), 
        playMove([Board ,player2|Rest], Mv, NewGameState),
        evaluate_board(NewGameState, player2,  Score)),
        [Score-Move | _]).

