?- set_prolog_flag(double_quotes,chars).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- dynamic numberB/1.
:- dynamic bestPath/4.
:- dynamic node/2.
:- dynamic link/3.

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


getValidMoves2(State,[]):- 
    getPlayerPieces(State,[0,_]),!.

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
    Score is (CapturedPieces1 - CapturedPieces2) * 100.

min_max(State,0):-
    evaluate_board(State,BestValue),
    getBoard(State,Board),
    %write('Value Was '),write(BestValue),nl,
    %printBoard(Board),nl,
    retractall(node(State,0)),
    asserta(node(State,BestValue)),!. 

min_max(State,Depth):-
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),[]),!,
    write('Olá\n'),nl,
    evaluate_board(State,BestValue),
    getBoard(State,Board),
    retract(node(State,0)),
    asserta(node(State,BestValue)).
min_max(State,Depth):-
    getPlayer(State, player1),!,
    write(player1),nl,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    setof([Value,Move,ChildState],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    write(Depth),write('Player '),write(player1),write('quer minimizar\n'),
    printAll(ValueMoves),
    findMin(ValueMoves,[V,M,C]),
    write('Escolheu o move que dá :'),write(V),nl,
    asserta(bestPath(C,State,M,V)),
    tryRetract(State),
    asserta(node(State,V)).
    
min_max(State,Depth):-
    getPlayer(State,P),write(P),nl,
    getBoard(State,Board),
    %printBoard(Board),nl,(ChildState,State,Move)),Children),!,
    findall(ChildState,(node(ChildState,Value),link(ChildState,State,Move)),Children),!,
    Depth1 is Depth - 1,
    min_max_list(Children,Depth1),!,
    setof([Value,Move,ChildState],(node(ChildState,Value),link(ChildState,State,Move)),ValueMoves),!,
    write(Depth),write('Player '),write(P),write('quer maximizar\n'),
    printAll(ValueMoves),
    findMax(ValueMoves,[V,M,C]),
    write('Chosen Move '),write([V]),nl,
    asserta(bestPath(C,State,M,V)),
    tryRetract(State),
    asserta(node(State,V)).

min_max_list([],_):-!.
min_max_list([Child|Children],Depth):-
    min_max(Child,Depth),
    min_max_list(Children,Depth).



tryRetract(State):-
    findall(Value,node(State,Value),[]),!.
tryRetract(State):-
    findall(Value,node(State,Value),Values),
    retractRec(State,Values).

retractRec(State,[]):-!.
retractRec(State,[V|VS]):-
    retract(node(State,V)),
    retractRec(State,VS).


mx(State, BestMove):-
    expand(State, 2),
    min_max(State,2),
    bestPath(C,State,BestMove,V),
    write(V),
    write(' Foi o escolhido\n'),

    abolish(node/2),
    abolish(link/3),
    abolish(bestPath/4).


greedyChoice([Board ,player2|Rest], Move, player2, Moves) :-

    setof(Score-Mv, NewGameState^Moves^(
        member(Mv, Moves), 
        playMove([Board ,player2|Rest], Mv, NewGameState),
        evaluate_board(NewGameState, player2,  Score)),
        [Score-Move | _]).
