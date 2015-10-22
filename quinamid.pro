:-use_module(library(lists)).
:-use_module(library(random)).

tabuleiro_2x2([[0,0], [0,0]]).
tabuleiro_3x3([[0,0,0], [0,0,0], [0,0,0]]).
tabuleiro_4x4([[0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0]]).
tabuleiro_5x5([[0,0,0,0,0], [0,0,0,0,0], [0,0,0,0,0], [0,0,0,0,0], [0,0,0,0,0]]).
tabuleiro_6x6([[0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0], [0,0,0,0,0,0]]).

limitesH(
	['                       ',
	 '--------               ',
	 '------------           ',
	 '----------------       ',
	 '--------------------   ']).

limitesV(
	[[' ','|','|','|','|'],
	 [' ','|','|','|','|'],
	 [' ',' ','|','|','|'],
	 [' ',' ',' ','|','|'],
	 [' ',' ',' ',' ','|'],
	 [' ',' ',' ',' ',' ']]).

vitoria([[P,_,_,_,_,_], [_,P,_,_,_,_], [_,_,P,_,_,_], [_,_,_,P,_,_], [_,_,_,_,P,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,_,_,_], [_,P,_,_,_,_], [_,_,P,_,_,_], [_,_,_,P,_,_], [_,_,_,_,P,_], [_,_,_,_,_,P]], P).
vitoria([[_,_,_,_,_,_], [P,_,_,_,_,_], [_,P,_,_,_,_], [_,_,P,_,_,_], [_,_,_,P,_,_], [_,_,_,_,P,_]], P).
vitoria([[_,P,_,_,_,_], [_,_,P,_,_,_], [_,_,_,P,_,_], [_,_,_,_,P,_], [_,_,_,_,_,P], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,_,_,P], [_,_,_,_,P,_], [_,_,_,P,_,_], [_,_,P,_,_,_], [_,P,_,_,_,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,_,_,_], [_,_,_,_,P,_], [_,_,_,P,_,_], [_,_,P,_,_,_], [_,P,_,_,_,_], [P,_,_,_,_,_]], P).
vitoria([[_,_,_,_,_,_], [_,_,_,_,_,P], [_,_,_,_,P,_], [_,_,_,P,_,_], [_,_,P,_,_,_], [_,P,_,_,_,_]], P).
vitoria([[_,_,_,_,P,_], [_,_,_,P,_,_], [_,_,P,_,_,_], [_,P,_,_,_,_], [P,_,_,_,_,_], [_,_,_,_,_,_]], P).

vitoria([[P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_], [_,_,_,_,_,_]], P).
vitoria([[_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,_,_]], P).
vitoria([[_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,_]], P).

vitoria([[P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_], [P,_,_,_,_,_]], P).
vitoria([[_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_], [_,P,_,_,_,_]], P).
vitoria([[_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_], [_,_,P,_,_,_]], P).
vitoria([[_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_], [_,_,_,P,_,_]], P).
vitoria([[_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_], [_,_,_,_,P,_]], P).
vitoria([[_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P], [_,_,_,_,_,P]], P).

vitoria([_,P,P,P,P,P], P).
vitoria([P,P,P,P,P,_], P).

limpa_ecra:-
	nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl, nl.

cabecalho:-
	limpa_ecra,
	write('==========================================='), nl, nl,
	write('  QUINAMID - PROLOG'), nl,
	write('==========================================='), nl, nl, nl.

jogador1(1).
jogador2(2).

iniciar:-
	cabecalho,
	escolhe_jogo(Modo),

	tabuleiro_2x2(A),
	tabuleiro_3x3(B),
	tabuleiro_4x4(C),
	tabuleiro_5x5(D),
	tabuleiro_6x6(E),

	limitesH(LimH), limitesV(LimV),
	mostra_tabuleiro(E, LimH, LimV),
	turno_jogador1([A,B,C,D,E], [0,0,0,0,0], LimH, LimV, Modo).


introduzir_coordenadas(7, 7, 1).
introduzir_coordenadas(Linha, Coluna, 2):-
	write('Linha :- '),
	get_code(TempX), Linha is TempX-48, get_char(_),
	write('Coluna :- '),
	get_code(TempY), Coluna is TempY-48, get_char(_).

introduzir_jogada(Linha, Coluna) :-
	write('Escolha uma das seguintes opcoes:'), nl,
	write('  1 - Mover tabuleiro'), nl,
	write('  2 - Jogar peca'), nl,
	get_code(Input), Opcao is Input-48, get_char(_),
	introduzir_coordenadas(Linha, Coluna, Opcao).

editar_matriz(0, Y, NovoElemento, [Linha|Resto], [NovaLinha|Resto]):-
	editar_linha(Linha, Y, NovoElemento, NovaLinha).

editar_matriz(X, Y, NovoElemento, [Linha|Resto], [Linha|Edicao]):-
	X > 0,
	NovoX is X-1,
	editar_matriz(NovoX, Y, NovoElemento, Resto, Edicao).

editar_linha([Elem|Direita], 0, NovoElemento, [NovoElemento|Direita]):-
	Elem == 0.

editar_linha([Elem|Direita], N, NovoElemento, [Elem|NovaDireita]):-
	N > 0,
	NovoN is N-1,
	editar_linha(Direita, NovoN, NovoElemento, NovaDireita).

jogar_peca(Linha, Coluna, Jogador, Tabuleiro, NovoTabuleiro):-
	editar_matriz(Linha, Coluna, Jogador, Tabuleiro, NovoTabuleiro).


devolve_elemento_aux([Elem|_],0,Elem).

devolve_elemento_aux([_Inicio|Resto],Coluna,Elem):-
Coluna > 0,
NC is Coluna-1,
devolve_elemento_aux(Resto,NC,Elem).

devolve_elemento([Linha|_Resto],0,Coluna,Elem):-
devolve_elemento_aux(Linha,Coluna,Elem).

devolve_elemento([_Inicio|Resto],Linha,Coluna,Elem):-
	Linha > 0,
	NL is Linha-1,
	devolve_elemento(Resto,NL,Coluna,Elem).

verifica_jogada(Tabuleiro,Linha, Coluna):-
	devolve_elemento(Tabuleiro,Linha,Coluna,X),
	write('X = '),write(X),nl,
	X == 0.

jogaCPU(Tabuleiro, Linha, Coluna):-
	random(0,6,Linha),write('Linha: '),write(Linha),nl,
	random(0,6,Coluna),write('Coluna: '),write(Coluna),nl,
	verifica_jogada(Tabuleiro, Linha, Coluna), !.

jogaCPU(Tabuleiro, Linha, Coluna):-
	jogaCPU(Tabuleiro, Linha, Coluna).


turno_jogador1(Tabuleiros, Posicoes, LimH, LimV, Modo):-
	jogador1(Jogador),

	nl, write('Jogador 1 :'), nl,
	introduzir_jogada(Linha, Coluna),

	processa_jogada(Linha, Coluna, Jogador, Tabuleiros, Posicoes, Resultado, NovasPosicoes),
	construir_tabuleiro(3, Resultado, NovoTabuleiro, NovasPosicoes),

	cabecalho,
	mostra_tabuleiro(NovoTabuleiro, LimH, LimV),
	verifica_vitoria(NovoTabuleiro, Jogador),
	turno_jogador2(Resultado, NovasPosicoes, LimH, LimV, Modo).

turno_jogador2(Tabuleiros, Posicoes, LimH, LimV, 1):-
	jogador2(Jogador),

	nl, write('Jogador 2 :'), nl,
	introduzir_jogada(Linha, Coluna),

	processa_jogada(Linha, Coluna, Jogador, Tabuleiros, Posicoes, Resultado, NovasPosicoes),
	construir_tabuleiro(3, Resultado, NovoTabuleiro, NovasPosicoes),

	cabecalho,
	mostra_tabuleiro(NovoTabuleiro, LimH, LimV),
	verifica_vitoria(NovoTabuleiro, Jogador),
	turno_jogador1(Resultado, NovasPosicoes, LimH, LimV, 1).

turno_jogador2(Tabuleiros, Posicoes, LimH, LimV,0):-
	jogador2(Jogador),

	construir_tabuleiro(3, Tabuleiros, Tabuleiro, Posicoes),
	jogaCPU(Tabuleiro, Linha, Coluna),
	processa_jogada(Linha, Coluna, Jogador, Tabuleiros, Posicoes, Resultado, NovasPosicoes),
	construir_tabuleiro(3, Resultado, NovoTabuleiro, NovasPosicoes),

	cabecalho,
	mostra_tabuleiro(NovoTabuleiro, LimH, LimV),
	verifica_vitoria(NovoTabuleiro, Jogador),
	turno_jogador1(Resultado, Posicoes, LimH, LimV, 0).

verifica_vitoria(Tabuleiro, Jogador):-
	vence(Tabuleiro, Jogador),
	nl, write('O jogador '), write(Jogador), write(' e o vencedor!'),
	get_char(_),
	break.

verifica_vitoria(_,_).

vence(Tabuleiro, Jogador):-
	vitoria(Tabuleiro, Jogador).

vence([_|Resto], Jogador):-
	vitoria(Resto, Jogador).

vence(Tabuleiro, Jogador):-
	vence(Tabuleiro, Jogador, 6).

vence([Linha|_], Jogador, _):-
	vitoria(Linha, Jogador).

vence([_|Resto], Jogador, N):-
	N>0,
	NovoN is N-1,
	vence(Resto, Jogador, NovoN).
	
soma_posicoesX([],0).
soma_posicoesX([Elem|Restantes], Soma):-
	soma_posicoesX(Restantes, NovaSoma),
	Soma is Elem mod 2 + NovaSoma.

soma_posicoesY([],0).
soma_posicoesY([Elem|Restantes], Soma):-
	soma_posicoesY(Restantes, NovaSoma),
	Soma is Elem // 2 + NovaSoma.

calcula_posicoes([],[],[]).
calcula_posicoes([Elem|Restantes], [SomaX|ProximasX], [SomaY|ProximasY]):-
	soma_posicoesX([Elem|Restantes], SomaX),
	soma_posicoesY([Elem|Restantes], SomaY),
	calcula_posicoes(Restantes, ProximasX, ProximasY).
	

coloca_peca([Linha|Coluna], Jogador, [Tabuleiro|OutrosTabuleiros], [PosicaoX|_], [PosicaoY|_], [TabuleiroResultado|OutrosTabuleiros], Tamanho):-
	Linha > PosicaoY,
	Linha =< PosicaoY+Tamanho,
	Coluna > PosicaoX,
	Coluna =< PosicaoX+Tamanho,

	LinhaTabuleiroResultado is Linha-PosicaoY-1,
	ColunaTabuleiroResultado is Coluna-PosicaoX-1,
	jogar_peca(LinhaTabuleiroResultado, ColunaTabuleiroResultado, Jogador, Tabuleiro, TabuleiroResultado).

coloca_peca([Linha|Coluna], Jogador, [Tabuleiro|OutrosTabuleiros], [_|OutrasPosicoesX], [_|OutrasPosicoesY], [Tabuleiro|Resultado], Tamanho):-
	NovoT is Tamanho+1,
	coloca_peca([Linha|Coluna], Jogador, OutrosTabuleiros, OutrasPosicoesX, OutrasPosicoesY, Resultado, NovoT).



introduzir_movimento(1,[_|Outras],[NovaPosicao|Outras]):-
	write('Onde pretende posicionar o tabuleiro?'), nl,
	write('  1 - Canto superior esquerdo'), nl,
	write('  2 - Canto superior direito'), nl,
	write('  3 - Canto interior esquerdo'), nl,
	write('  4 - Canto inferior direito'), nl,
	get_code(Input), NovaPosicao is Input-49, get_char(_).

introduzir_movimento(N, [Posicao|Outras], [Posicao|Restantes]):-
	NovoN is N-1,
	introduzir_movimento(NovoN, Outras, Restantes).

movimento_possivel(Tabuleiros, Posicoes, NovasPosicoes):-
	Posicoes == NovasPosicoes,
	limpa_ecra, write('Movimento invalido!'), nl, nl,
	processa_jogada(7,7,0,Tabuleiros,Posicoes,Tabuleiros, NovasPosicoes).
movimento_possivel(_,_,_).

processa_jogada(7,7,_,Tabuleiros,Posicoes,Tabuleiros, NovasPosicoes):-
	write('Que tabuleiro pretende mover?'), nl,
	write('  1 - 2x2'), nl,
	write('  2 - 3x3'), nl,
	write('  3 - 4x4'), nl,
	write('  4 - 5x5'), nl,
	get_code(Input), N is Input-48, get_char(_),
	introduzir_movimento(N, Posicoes, NovasPosicoes),
	movimento_possivel(Tabuleiros, Posicoes, NovasPosicoes).

processa_jogada(Linha, Coluna, Jogador, Tabuleiros, Posicoes, Resultado, Posicoes):-
	calcula_posicoes(Posicoes, PosX, PosY),
	coloca_peca([Linha|Coluna], Jogador, Tabuleiros, PosX, PosY, Resultado, 2).



escolhe_jogo(Modo):-
	write('1- Jog/PC; 2- Jog/Jog.'), nl,
	repeat, get_code(Opc), Opc>=49, Opc=<50,
	Modo is Opc-49, get_char(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%REPRESENTAÇÃO DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%%%%

mostra_tabuleiro(Tab, LimH, LimV):-
	write('    1   2   3   4   5   6'), nl,
	write('  |-----------------------|'), nl,
	mostra_linhas(1, Tab, LimH, LimV),
	write('  |-----------------------|'), nl.

mostra_linhas(7,[],[],[]).

mostra_linhas(N,[Linha|Resto], LimH, [Lims|NextLims]):-
	write(N), write(' |'),
	escreve_linha(6, Linha, Lims),
	write('|'),
	NewN is N+1,
	desenhaLimiteH(NewN, Resto, LimH, NextLims).

desenhaLimiteH(7, [],[],[]):-nl.

desenhaLimiteH(N, Lines, [Lim|NextLim], LimV):-
	nl, write('  |'),
	write(Lim),
	write('|'), nl,
	mostra_linhas(N, Lines, NextLim, LimV).

escreve_linha(N, [Elem|Next], LimV):-
	write(' '), escreve(Elem), write(' '),
	NewN is N-1,
	desenhaLimiteV(NewN, Next, LimV).

desenhaLimiteV(0,[],[]).

desenhaLimiteV(N, Line, [Lim|NextLim]):-
	write(Lim),
	escreve_linha(N, Line, NextLim).

escreve(0):-
		write(' ').

escreve(1):-
		write('X').

escreve(2):-
		write('O').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%REPRESENTAÇÃO DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%%%%

ultimo_elemento(1, Elem, Elem).

ultimo_elemento(Tamanho, [_|NovoTabuleiroLinha], Elem):-
	NovoT is Tamanho-1,
	ultimo_elemento(NovoT, NovoTabuleiroLinha, Elem).




construir_linha(Tamanho, 1, _, ProximoTabuleiroLinha, Elem, 0):-
	ultimo_elemento(Tamanho, ProximoTabuleiroLinha, Elem).

construir_linha(Tamanho, N, [Elem|Proximos], ProximoTabuleiroLinha, [Elem|Resultado], 0):-
	NovoN is N-1,
	construir_linha(Tamanho, NovoN, Proximos, ProximoTabuleiroLinha, Resultado, 0).

construir_linha(Tamanho, 1, _, ProximoTabuleiroLinha, Elem, 2):-
	ultimo_elemento(Tamanho, ProximoTabuleiroLinha, Elem).

construir_linha(Tamanho, N, [Elem|Proximos], ProximoTabuleiroLinha, [Elem|Resultado], 2):-
	NovoN is N-1,
	construir_linha(Tamanho, NovoN, Proximos, ProximoTabuleiroLinha, Resultado, 2).

construir_linha(Linha, [Elem|_], [Elem|Linha], 1).
construir_linha(Linha, [Elem|_], [Elem|Linha], 3).




construir_linhas(_, 1, _, Linha, Linha, 0).
construir_linhas(_, 1, _, Linha, Linha, 1).
construir_linhas(_,0,_,_,[],2).
construir_linhas(_,0,_,_,[],3).


construir_linhas(Tamanho, N, [Linha|Proximas], [TabuleiroLinha|ProximoTabuleiroLinha], [LinhaResultado|ProximasLinhasResultado], 0):-
	NovoN is N-1,
	construir_linha(Tamanho,  Tamanho, Linha, TabuleiroLinha, LinhaResultado, 0),
	construir_linhas(Tamanho, NovoN, Proximas, ProximoTabuleiroLinha, ProximasLinhasResultado, 0).

construir_linhas(Tamanho, N, [Linha|Proximas], [TabuleiroLinha|ProximoTabuleiroLinha], [LinhaResultado|ProximasLinhasResultado], 1):-
	NovoN is N-1,
	construir_linha(Linha, TabuleiroLinha, LinhaResultado, 1),
	construir_linhas(Tamanho, NovoN, Proximas, ProximoTabuleiroLinha, ProximasLinhasResultado, 1).

construir_linhas(Tamanho, Tamanho, Tabuleiro, [TabuleiroLinha|ProximoTabuleiroLinha], [TabuleiroLinha|ProximasLinhasResultado], 2):-
	NovoN is Tamanho-1,
	construir_linhas(Tamanho, NovoN, Tabuleiro, ProximoTabuleiroLinha, ProximasLinhasResultado, 2), !.

construir_linhas(Tamanho, N, [Linha|Proximas], [TabuleiroLinha|ProximoTabuleiroLinha], [LinhaResultado|ProximasLinhasResultado], 2):-
	NovoN is N-1,
	construir_linha(Tamanho,  Tamanho, Linha, TabuleiroLinha, LinhaResultado, 2),
	construir_linhas(Tamanho, NovoN, Proximas, ProximoTabuleiroLinha, ProximasLinhasResultado, 2).

construir_linhas(Tamanho, Tamanho, Tabuleiro, [TabuleiroLinha|ProximoTabuleiroLinha], [TabuleiroLinha|ProximasLinhasResultado], 3):-
	NovoN is Tamanho-1,
	construir_linhas(Tamanho, NovoN, Tabuleiro, ProximoTabuleiroLinha, ProximasLinhasResultado, 3), !.

construir_linhas(Tamanho, N, [Linha|Proximas], [TabuleiroLinha|ProximoTabuleiroLinha], [LinhaResultado|ProximasLinhasResultado], 3):-
	NovoN is N-1,
	construir_linha(Linha, TabuleiroLinha, LinhaResultado, 3),
	construir_linhas(Tamanho, NovoN, Proximas, ProximoTabuleiroLinha, ProximasLinhasResultado, 3).

construir_tabuleiro(7, [Tabuleiro|_], Tabuleiro, _).

construir_tabuleiro(Tamanho, [Tabuleiro, ProximoTabuleiro|Outros], Resultado, [Posicao|Proximas]):-
	NovoT is Tamanho+1,
	construir_linhas(Tamanho, Tamanho, Tabuleiro, ProximoTabuleiro, Soma, Posicao),
	construir_tabuleiro(NovoT, [Soma|Outros], Resultado, Proximas).
