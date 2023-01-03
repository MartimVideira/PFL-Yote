% Helper Functions that are of general use

myRepeat(_,0,[]):-!.
myRepeat(X,N,[X|XS]):- 
    N1 is N - 1,
    myRepeat(X,N1,XS).

% Prints lists
printList([]).
printList([X|XS]):- write(X),printList(XS).


printN(0,_):-!.
printN(N,X):- write(X), N1 is N - 1, printN(N1,X).

% Concatenates two lists and returns them
myConcat([],YS,YS).
myConcat([X|XS],YS,[X|ZS]):- myConcat(XS,YS,ZS).


myIntersperse([],_,[]).
myIntersperse([X|XS],YS,Res):-
    ThisRes = [X | YS],
    myIntersperse(XS,YS,Res1),
    myConcat(ThisRes,Res1,Res).


% retrieves element at position
at(0,[X|_],X):-!.
at(N,[_|XS],Elem):- N1 is N -1 , at(N1,XS,Elem).

myTake(0,XS,[]):-!.
myTake(N,[X|XS],[X|YS]):-
    N1 is N - 1,
    myTake(N1,XS,YS).

% sets element at position
setAt(0,[_|XS],Y,[Y|XS]):-!.
setAt(N,[X|XS],Y,[X|ZS]):-
    N1 is N - 1,
    setAt(N1,XS,Y,ZS).

printAll([]).
printAll([X|XS]):-
    write(X),nl,printAll(XS).

printValues([]).
printValues([X-_-_|XS]):-
    write(X),nl,printValues(XS).

%finds max element
findMax([[V,C,M]],[V,C,M]):-!.
findMax([[V,C,M]|XS], [VC,CC,MC]):- findMax(XS, [VC,CC,MC]), VC >= V.
findMax([[V,C,M]|XS],[V,C,M]):- findMax(XS, [VC,CC,MM]), V >  VC.

%finds min element
findMin([[V,C,M]],[V,C,M]) :- !.
findMin([[V,C,M]|XS], [VC,CC,MC]):- findMin(XS, [VC,CC,MC]), VC < V.
findMin([[V,C,M]|XS], [V,C,M]):- findMin(XS, [VC,CC,MM]), V =<  VC.