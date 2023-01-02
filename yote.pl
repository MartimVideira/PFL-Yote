?- set_prolog_flag(double_quotes,chars).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- consult('ai.pl').



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

playRoundDumbAI(State):-
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

