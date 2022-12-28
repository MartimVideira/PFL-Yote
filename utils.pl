% Helper Functions

% Creates in XS a list of N Xs.
myRepeat(_,0,[]):-!.
myRepeat(X,N,[X|XS]):- 
    N1 is N - 1,
    myRepeat(X,N1,XS).

printList([]).
printList([X|XS]):- write(X),printList(XS).

printN(0,_):-!.
printN(N,X):- write(X), N1 is N - 1, printN(N1,X).

myConcat([],YS,YS).
myConcat([X|XS],YS,[X|ZS]):- myConcat(XS,YS,ZS).

myIntersperse([],_,[]).
myIntersperse([X|XS],YS,Res):-
    ThisRes = [X | YS],
    myIntersperse(XS,YS,Res1),
    myConcat(ThisRes,Res1,Res).

at(0,[X|_],X):-!.
at(N,[_|XS],Elem):- N1 is N -1 , at(N1,XS,Elem).

setAt(0,[_|XS],Y,[Y|XS]):-!.
setAt(N,[X|XS],Y,[X|ZS]):-
    N1 is N - 1,
    setAt(N1,XS,Y,ZS).
