?- set_prolog_flag(double_quotes,chars).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- dynamic numberB/1.

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



% min_max(Player, State, BestMove): -
%     expand(State, 3).



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

  
evaluate_board([Board, player2|Rest], Score):-
    getPlayerPieces([Board,player2|Rest], [P2,_]),
    getPlayerPieces([Board,player1|Rest], [_,_]),
    Score = P2.

evaluate_board([Board, player1|Rest], Score):-
    getPlayerPieces([Board,player2|Rest], [_,N]),
    getPlayerPieces([Board,player1|Rest], [_,M]),
    Score = 10 * M - (10 * N).

% node(State,Value)
% link(State,[State,Move])
numberB(1, 1).

printNumber(X, Y):-
    numberB(1, Y),
    X = X + 1.  


min_max(State,0):-
    evaluate_board(State,BestValue),
    retract(node(State,0)),
    asserta(node(State,BestValue)).

min_max(State,Depth):-
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),
    Children == [],!.
min_max(State,Depth):-
    getBoard(State,Board),
    printBoard(Board),nl,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    findall(Value-Move,(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    write(ValueMoves),nl.
    



min_max_list([],_):-!.
min_max_list([Child|Children],Depth):-
    min_max(Child,Depth),!,
    min_max_list(Children,Depth).
