?- set_prolog_flag(double_quotes,chars).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- dynamic numberB/1.
:- dynamic bestPath/3.

getValidMoves(State, Moves):-
    getValidMoves2(State, Moves2),
    getValidMoves1(State, Moves1),
    myConcat(Moves1, Moves2, Moves).

getValidMoves1(State, Moves):- 
    numberColumns(NC),
    numberLines(NL),
    setof([Ci, Li, Cf, Lf], (Notation, State)^
(   between(0, NC, Ci),
    between(0, NL, Li),
    between(0, NC, Cf),
    between(0, NL, Lf),
    notationToInts(Notation, [Ci, Li, Cf, Lf]),
    isValidMove(State, Notation)), Moves) ; Moves = [].

getValidMoves2(State, Moves):- 
    numberColumns(NC),
    numberLines(NL),
        setof([C, L], (Notation, State)^
(   between(0, NC, C),
    between(0, NL, L),
    notationToInts(Notation, [C, L]),
    isValidMove(State, Notation)),
    Moves); Moves =[].


createNodes(_,[]):-!.
createNodes(Parent,[Move|Moves]):-
    playMove(Parent,Move,[Board,Player|Rest]),
    nextPlayer(Player,NextPlayer),
    asserta(node([Board,NextPlayer|Rest],0)),
    asserta(link([Board,NextPlayer|Rest],Parent,Move)),!,
    createNodes(Parent,Moves).



expand(_,0):-!.
expand(State,Depth):-
    getValidMoves(State,Moves),
    createNodes(State,Moves),!,
    Depth1 is Depth - 1,
    setof(Child,V^node(Child,V),Children),!,
    expandChildren(Children,Depth1),!.
    
expandChildren([],_):-!.
expandChildren(_,0):-!.
expandChildren([Child|Rest],Depth):-
    expand(Child,Depth),!,
    expandChildren(Rest,Depth).

  
evaluate_board([Board, _|Rest], Score):-
    getPlayerPieces([Board, player2|Rest], [_,CapturedPieces2]),
    getPlayerPieces([Board, player1|Rest], [_,CapturedPieces1]),
    Score is (CapturedPieces2 - CapturedPieces1) * 100.



min_max(State,0):-
    evaluate_board(State,BestValue),
    getBoard(State,Board),
    %printBoard(Board),nl,
    retract(node(State,0)),
    asserta(node(State,BestValue)).

min_max(State,Depth):-
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),
    Children == [],!.
min_max(State,Depth):-
    getPlayer(State, player1),!,
    getBoard(State,Board),
    %printBoard(Board),nl,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    findall([ChildState,Value,Move],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    at(0, ValueMoves,[C,V,M]),
    asserta(bestPath(C,State,M)).
    
min_max(State,Depth):-
    getBoard(State,Board),
    %printBoard(Board),nl,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    findall([ChildState,Value,Move],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    length(ValueMoves, L),
    L1 is L -1 ,
    at(L1, ValueMoves,[C,V,M]),
    asserta(bestPath(C,State,M)).

min_max_list([],_):-!.
min_max_list([Child|Children],Depth):-
    min_max(Child,Depth),
    min_max_list(Children,Depth).


mx(State):-
    expand(State, 2),
    min_max(State,2),
    bestPath(C,State,BestMove),
    write(BestMove),!.
