:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

minimax([Board ,player2|Rest], Move, player2, Moves, Depth) :-

        setof(Score-Mv, NewGameState^Moves^(
        member(Mv, Moves), 
        playMove([Board ,player2|Rest], Mv, NewGameState),
        evaluate_board(NewGameState, player2,  Score)),
        [Score-Move | _]).

minimax(Board, BestMove) :-
	minimax_step(max, Board, BestMove, _).

minimax_step(MinMax, Board, BestMove, BestValue) :-
	player_color(MinMax, Color),
	all_possible_moves(Color, Board, AllMoves),
    best_move(MinMax, AllMoves, BestMove, BestValue).


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



MinMax(State) -> ([States])




(preto) = -1000
(branco) = -1000


evaluateMinMax([Board,_|Rest], Score):-

    getPlayerPieces([Board,player2|Rest], [_,N]),
    getPlayerPieces([Board,player1|Rest], [_,M]),
    Score = (10 * M - (10 * N)).