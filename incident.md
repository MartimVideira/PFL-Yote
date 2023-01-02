# TEMOS UM PROBLEMA

Então é o seguinte. Na função **playRoundSmartAI**, o nosso AI tenta fazer uma escolha inteligente e chama a função **smartChoice**. A esta função, é passado o set de movimentos que ele pode concretizar. 

*Mas é aqui que está o problema*.


## 1a Parte do Problema

Vamos olhar para a função **playRoundSmartAI**:
```prolog
    getValidMoves([Board, player2|Rest], Moves),
    smartChoice([Board ,player2|Rest], MoveAI, player2, Moves),
```

Como podes ver, os movimentos são gerados na função **getValidMoves**. No entanto, esta função tem **duas** versões:
```prolog
getValidMoves(State, Moves):- 
    setof([Ci, Li, Cf, Lf], (Notation, State)^
(   between(0, 5, Ci),
    between(0, 4, Li),
    between(0, 5, Cf),
    between(0, 4, Lf),
    notationToInts(Notation, [Ci, Li, Cf, Lf]),
    isValidMove(State, Notation)), Moves).

getValidMoves(State, Moves):- 
        setof([C, L], (Notation, State)^
(   between(0, 5, C),
    between(0, 4, L),
    notationToInts(Notation, [C, L]),
    isValidMove(State, Notation)),
    Moves).
```

É aqui que está o problema. A função gera primeiro o set de movimentos que consiste em mover uma peça. No momento em que é possível mover peças (depois da primeira ronda), o set de movimentos gerado é **unicamente de movimentos do tipo [Ci, Li, Cf, Lf], ou seja, movimentações de peças**, porque a função encontra retorna "true" e já não pesquisa no set de movimentos de colocar peças em campo.


## 2a Parte do Problema

A função **smartChoice** também tem duas variantes. Tal que uma é chamada apenas se a outra não resultar. Isto é problemático, porque desta forma, o jogador tenta sempre mover peças antes de colocar alguma em campo.


# Minha sugestão

Temos de arranjar forma de combinar todos estes movimentos. Uma forma possível é **tornar o movimento [C, L] em movimentos [C, L, C, L]**. é chato mas talvez funcione. Outra maneira seria arranjar forma de chamar ambas as funções mas não sei como isso se faz. Até amanha! 