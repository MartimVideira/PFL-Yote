best_move(max, [], [], -2).
best_move(min, [], [], 2).


change_max_min(max, min).
change_max_min(min, max).

eval_board(State, Score):- 
    Score = 2.
  

compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :-
    evaluate_board(Move, Value),
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
	compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).


best_move(MinMax, [Move | RestMoves], BestMove, BestValue) :-
	best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV),
	change_max_min(MinMax, Other),
	minimax_step(Other, Move, _, BottomBestV),
	compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).

getValidBoards(State, Boards):-
    getValidMoves(State, Moves),

    setof(NewGameState, NewGameState^Moves^(
        member(Mv, Moves),
        playMove([Board ,player2|Rest], Mv, NewGameState),
        evaluate_board(NewGameState, player2,  Score)),
        [Boards | _]).



minimax_step(MinMax, Board, BestMove, BestValue) :-
	expand(Board, Moves),
    best_move(MinMax, Moves, BestMove, BestValue).

% minimax(+Board, -BestMove)
% Matches the next move based on the current board.
minimax(Board, BestBoard) :-
	minimax_step(max, Board, BestBoard, _).