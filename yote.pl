?- set_prolog_flag(double_quotes,chars).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- consult('ai.pl').


playRound(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!'),nl,nl.

playRound(State):-
    display_game(State),
    getPlayerMove(State,Move),
    notationToInts(Move,ConvertedMove),
    move(State,ConvertedMove,[NewBoard,Player|Rest]),!,
    nextPlayer(Player,NextPlayer),
    playRound([NewBoard,NextPlayer|Rest]),!.


playGame:-
    initial_state(S),
    playRound(S).


playDumbAI:-
    initial_state(S),
    playRoundDumbAI(S).

playRoundDumbAI(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').
    
playRoundDumbAI([Board, player2|Rest]):-

    valid_moves([Board ,player2|Rest], Moves),
    random_member(MoveAI, Moves),
    write(MoveAI),
    move([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundDumbAI([NewBoard,player1|NewRest]).

playRoundDumbAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),!,
    write('Played Human Move'),
    playRoundDumbAI([NewBoard,player2|NewRest]).

playRoundHuman([Board, player1|Rest],NewState):-
    display_game([Board, player1|Rest]),
    getPlayerMove([Board, player1|Rest],Move),
    notationToInts(Move,ConvertedMove),
    move([Board, player1|Rest],ConvertedMove,NewState).

playAI:-
    initial_state(State),
    playRoundAI(State).

playRoundAI(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').

playRoundAI([Board, player2|Rest]):-
    minimax_choice([Board ,player2|Rest], MoveAI),
    move([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundAI([NewBoard,player1|NewRest]).

playRoundAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),
    playRoundAI([NewBoard,player2|NewRest]).

playAIAI:-
    initial_state(State),
    playRoundAIAI(State).

playRoundAIAI(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').
playRoundAIAI([Board,Player|Rest]):-
    display_game([Board,Player|Rest]),
    minimax_choice([Board ,Player|Rest], MoveAI),
    nextPlayer(Player,NextPlayer),
    move([Board,Player|Rest],MoveAI,[NewBoard,Player|NewRest]),!,
    playRoundAIAI([NewBoard,NextPlayer|NewRest]).

playGreedy:-


playMmGreedy:-
    initial_state(State),
    playRoundMmGreedy(State).

playRoundMmGreedy(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').
playRoundMmGreedy([Board,player1|Rest]):-
    display_game([Board,player1|Rest]),
    minimax_choice([Board ,player1|Rest], MoveAI),
    move([Board,player1|Rest],MoveAI,[NewBoard,player1|NewRest]),!,
    playRoundMmGreedy([NewBoard,player2|NewRest]).
playRoundMmGreedy([Board,player2|Rest]):-
    display_game([Board,player2|Rest]),
    valid_moves([Board,player2|Rest],Moves),
    greedy_choice([Board,player2|Rest],MoveAI,Moves),
    sleep(0.5),
    move([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundMmGreedy([NewBoard,player1|NewRest]).

clearGameConfig:-
    abolish(numberColumns/1),
    abolish(numberLines/1),
    abolish(numberPieces/1).

setGameConfig(Columns,Lines,Pieces):-
    asserta(numberColumns(Columns)),
    asserta(numberLines(Lines)),
    asserta(numberPieces(Pieces)).
getGameConfig(Columns,Lines,Pieces):-
    numberColumns(Columns),
    numberLines(Lines),
    numberPieces(Pieces).

normalBoard:-
    clearGameConfig,
    setGameConfig(6,5,12).
aiBoard:-
    clearGameConfig,
    setGameConfig(4,4,4).

playerChooseGameConfig:-
    clearGameConfig,
    write('Número de peças: '),
    read(Pieces),
    write('Número de colunas: '),
    read(Columns),
    write('Número de linhas: '),
    read(Lines),
    setGameConfig(Columns,Lines,Pieces),!.

playerChooseGameConfig:- playerChooseGameConfig.

%Game Menu
menu:-
    write('----------\n'),
    write('     Yote\n'),
    write('----------\n'),
    write('0) Jogar Contra Jogador.\n'),
    write('1) Jogar Contra AI - Dumb.\n'),
    write('2) Jogar Contra AI - Greedy.\n'),
    write('3) Jogar Contra AI - Minimax.\n'),
    write('5) Ver Minimax vs Minimax\n'),
    write('6) Ver Minimax vs Greedy\n'),
    write('7) Alterar Configurações do Jogo.\n'),
    write('8) Sair.\n'),
    read(Option),
    menu(Option).
menu(8):-
    write('Obrigado!').
menu(7):-
    write('Deseja Escolher As Configurações Do Jogo? \n'),
    write('0) Não.\n'),
    write('1) Sim.\n'),
    read(Option),
    menuGameConfig(Option).
menu(6):-
    getGameConfig(C,L,P),
    aiBoard,!,
    playMmGreedy,
    setGameConfig(C,L,P),
    menu.
menu(4):-
    write('Jogando Contra AI vs AI\n'),
    getGameConfig(C,L,P),
    aiBoard,
    playAIAI,
    setGameConfig(C,L,P),
    menu.
menu(3):-
    write('Jogando Contra AI - Minimax\n'),
    getGameConfig(C,L,P),
    aiBoard,
    playAI,
    setGameConfig(C,L,P),
    menu.
menu(2):-
    write('Jogando Contra AI - Minimax\n'),
    getGameConfig(C,L,P),
    aiBoard,
    playAI,
    setGameConfig(C,L,P),
    menu.
menu(0):-
    write('Jogando Contra Jogador\n'),
    playGame,
    menu.
menuGameConfig(1):-
    playerChooseGameConfig,
    menu.
menuGameConfig(2):-
    menu.
menu(N):-
    write('Escolha Inválida'),
    menu.

play:- menu.