?- set_prolog_flag(double_quotes,chars).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').



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

