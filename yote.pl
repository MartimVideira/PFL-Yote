?- set_prolog_flag(double_quotes,chars).
?- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- consult('utils.pl').
:- consult('state.pl').
:- consult('io.pl').
:- consult('move.pl').
:- consult('ai.pl').


playRound(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').

playRound(State):-
    display_game(State),
    getPlayerMove(State,Move),
    notationToInts(Move,ConvertedMove),
    playMove(State,ConvertedMove,[NewBoard,Player|Rest]),!,
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
    playMove([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundDumbAI([NewBoard,player1|NewRest]).

playRoundDumbAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),!,
    write('Played Human Move'),
    playRoundDumbAI([NewBoard,player2|NewRest]).

playRoundHuman([Board, player1|Rest],NewState):-
    display_game([Board, player1|Rest]),
    getPlayerMove([Board, player1|Rest],Move),
    notationToInts(Move,ConvertedMove),
    playMove([Board, player1|Rest],ConvertedMove,NewState).

playAI:-
    initial_state(State),
    playRoundAI(State).

playRoundAI(State):-
    game_over(State,Winner),!,
    write('Player '),write(Winner),write(' won the Game!').

playRoundAI([Board, player2|Rest]):-
    minimax_choice([Board ,player2|Rest], MoveAI),
    write(MoveAI),
    playMove([Board,player2|Rest],MoveAI,[NewBoard,player2|NewRest]),!,
    playRoundAI([NewBoard,player1|NewRest]).

playRoundAI([Board,player1|Rest]):-
    playRoundHuman([Board,player1|Rest],[NewBoard,player1|NewRest]),
    playRoundAI([NewBoard,player2|NewRest]).


clearGameConfig:-
    retractall(numberColumns/1),
    retractall(numberLines/1),
    retractall(numberPieces/1).

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
    setGameConfig(3,3,2).

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


menu:-
    write('----------\n'),
    write('     Yote\n'),
    write('----------\n'),
    write('1) Jogar Contra Jogador.\n'),
    write('2) Jogar Contra AI.\n'),
    write('3) Alterar Configurações do Jogo.\n'),
    write('4) Sair.\n'),
    read(Option),
    menu(Option).
menu(4):-
    write('Obrigado!').
menu(3):-
    write('Deseja Escolher As Configurações Do Jogo? \n'),
    write('0) Não.\n'),
    write('1) Sim.\n'),
    read(Option),
    menuGameConfig(Option).
menu(2):-
    write('Jogando Contra AI\n'),
    getGameConfig(C,L,P),
    aiBoard,
    playAI,
    setGameConfig(C,L,P),
    menu.
menu(1):-
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