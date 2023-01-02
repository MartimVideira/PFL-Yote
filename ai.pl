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