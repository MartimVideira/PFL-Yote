?- set_prolog_flag(double_quotes,chars).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- dynamic numberB/1.
:- dynamic bestPath/4.
:- dynamic node/2.
:- dynamic link/3.



include_between:-
    current_prolog_flag(dialect, swi),!.
include_between:-
    use_module(library(between)).
:- include_between.
/**
 * valid_moves(+ State, - Moves)
 *
 * Generates every possible move given the state of the board
 * as well as the player who should be playing and their pieces,
 * all contained in the State argument. Those are returned in - Movesh
 */
valid_moves(State, Moves):-
    
    valid_moves_place(State, Moves2),
    valid_moves_change(State, Moves1),
    myConcat(Moves1, Moves2, Moves).

/**
 * valid_moves_change(+ State, - Moves)
 *
 * Generates every possible move given the state of the board,
 * but only the ones that consist in moving a piece from
 * one place to the other. Those are returned in - Moves.
 */
valid_moves_change(State, Moves):-
    numberColumns(NC),
    numberLines(NL),
    setof([Ci, Li, Cf, Lf], (Notation, State)^
(   between(0, NC, Ci),
    between(0, NL, Li),
    between(0, NC, Cf),
    between(0, NL, Lf),
    notationToInts(Notation, [Ci, Li, Cf, Lf]),
    isValidMove(State, Notation)), Moves) ; Moves = [].



valid_moves_place(State,[]):- 
    getPlayerPieces(State,[0,_]),!.


/**
 * valid_moves_place(+ State, - Moves)
 *
 * Generates every possible move given the state of the board,
 * but only the ones that consist in placing a piece on
 * the board. Those are returned in - Moves.
 */
valid_moves_place(State, Moves):- 
    numberColumns(NC),
    numberLines(NL),
        setof([C, L], (Notation, State)^
(   between(0, NC, C),
    between(0, NL, L),
    notationToInts(Notation, [C, L]),
    isValidMove(State, Notation)),
    Moves); Moves =[].


createNodes(_,[]):-!.

/**
 * createNodes(+ Parent, + [Move|Moves])
 *
 * Creates a representation of the board and the boards that could originate from it.
 * It creates a tree-like data structure so that it is easir to traverse when
 * performing a Min-Max search.
 */
createNodes(Parent,[Move|Moves]):-
    move(Parent,Move,[Board,Player|Rest]),
    nextPlayer(Player,NextPlayer),
    asserta(node([Board,NextPlayer|Rest],0)),
    asserta(link([Board,NextPlayer|Rest],Parent,Move)),!,
    createNodes(Parent,Moves).




expand(_,0):-!.

/**
 * expand(+ State, + Depth)
 *
 * Given a certain depth, it calls createNodes(+ Parent, + [Move|Moves]) to recursively
 * generate a tree-like data structure to represent the board and possible boards that may branch
 * from the given position.
 */
expand(State,Depth):-
    valid_moves(State,Moves),
    createNodes(State,Moves),!,
    Depth1 is Depth - 1,
    setof(Child,V^node(Child,V),Children),!,
    expandChildren(Children,Depth1),!.
    
expandChildren([],_):-!.
expandChildren(_,0):-!.
expandChildren([Child|Rest],Depth):-
    expand(Child,Depth),!,
    expandChildren(Rest,Depth).

/**
 * value(+ [Board, _|Rest], - Score)
 *
 * Given a certain State with board and player knowledge it evaluates the board
 * and returns its score via the argument - Score.
 */
value([Board, _|Rest], Score):-
    getPlayerPieces([Board, player2|Rest], [_,CapturedPieces2]),
    getPlayerPieces([Board, player1|Rest], [_,CapturedPieces1]),
    Score is (CapturedPieces1 - CapturedPieces2) * 100.

min_max(State,0):-
    value(State,BestValue),
    retractall(node(State,0)),
    asserta(node(State,BestValue)),!. 

min_max(State,_Depth):-
    findall(ChildState,(node(ChildState,_Value),link(ChildState,State,_Move)),[]),!,
    value(State,BestValue),
    getBoard(State,_Board),
    retract(node(State,0)),
    asserta(node(State,BestValue)).

/**
 * min_max(+ State, + Depth)
 *
 * Performs a Min-Max search on the given State (+ State) up until a certain Depth (+Depth).
 * Stores values assigned to each child-board on rules that we consult later to extract the one
 * that was evaluated with the highest score.
 */
min_max(State,Depth):-
    getPlayer(State, player1),!,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    setof([Value,Move,ChildState],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    findMin(ValueMoves,[V,M,C]),
    asserta(bestPath(C,State,M,V)),
    tryRetract(State),
    asserta(node(State,V)).
    
min_max(State,Depth):-
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    setof([Value,Move,ChildState],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    findMax(ValueMoves,[V,M,C]),
    asserta(bestPath(C,State,M,V)),
    tryRetract(State),
    asserta(node(State,V)).

min_max_list([],_):-!.
min_max_list([Child|Children],Depth):-
    min_max(Child,Depth),
    min_max_list(Children,Depth).


/**
 * tryRetract(+ State)
 *
 * Tries to retract all possible 'node' rules associated to a certain state.
 */
tryRetract(State):-
    findall(Value,node(State,Value),[]),!.
tryRetract(State):-
    findall(Value,node(State,Value),Values),
    retractRec(State,Values).

retractRec(_State,[]):-!.
retractRec(State,[V|VS]):-
    retract(node(State,V)),
    retractRec(State,VS).

/**
 * minimax_choice(+ State, - BestMove)
 *
 * Receives a State an performs a Min-Max search of depth 2.
 * Then, returns the best move found via the argument - Best Move.
 */
minimax_choice(State, BestMove):-
    expand(State, 2),
    min_max(State,2),
    bestPath(_C,State,BestMove,_V),
    abolish(node/2),
    abolish(link/3),
    abolish(bestPath/4).


greedy_choice([Board ,Player|Rest], Move, Moves) :-

    setof(Score-Mv, NewGameState^Moves^(
        member(Mv, Moves), 
        move([Board ,Player|Rest], Mv, NewGameState),
        value(NewGameState, Score)),
        [Score-Move | _]).
