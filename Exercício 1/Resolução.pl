%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Resolução do exercício 1 (Trabalho prático)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dados da base de conhecimento
utente(1,diogo,21,braga).
utente(2,rui,20,braga).
utente(3,esm,21,prado).
utente(4,miguel,22,viana).

instituicao( hpbraga ).
instituicao( hsjoao ).
instituicao( viana ).

cuidado(1,morreu, hpbraga, braga).
cuidado(2,partiu, hsjoao, porto).
cuidado(3,nascimento, hpbraga, braga).

ato(01-02-1996, 3,3,10).
ato(15-03-2017, 1,2,15).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}
%                       utente: IdUt -> {V,F}

%utente(Id) :- utente(Id,N,I,M).

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+utente(ID, N, I, M) :: (solucoes(ID, utente(ID, _, _, _), S),
                          comprimento(S, L), 
                          L == 1).

% Invariante Estrutural: garantir a remoção de conhecimento

-utente(ID, N, I, M) :: (solucoes(ID, utente(ID, _, _, _), S),
                          comprimento(S, L), 
                          L == 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado prestado: 
% IdServ, Descrição, Instituição, Cidade -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+cuidado(ID,D,I,C) :: (solucoes(ID, (cuidado(ID, _, _, _)), S),
                       comprimento(S, L), 
                       instituicao( I ),
                       L == 1).

% Invariante Estrutural: garantir a remoção de conhecimento

-cuidado(ID,D,I,C) :: (solucoes(ID, cuidado(ID, _, _, _), S),
                       comprimento(S, L),
                       instituicao( I ), 
                       L == 0).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado Instituição: 
% Nome -> {V,F}

+instituicao( Nome ) :: (solucoes(ID, instituicao(Nome), S),
                         comprimento(S, L), 
                         L == 1).

-instituicao( Nome ) :: (solucoes(ID, instituicao(Nome), S),
                         comprimento(S, L), 
                         L == 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: 
% Data, IdUtente, IdServico, Custo -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+ato(D,IDU,IDS,C) :: (solucoes(IDU, utente(IDU,_,_,_), S),
                      comprimento(S, L), 
                      L == 1).	

+ato(D,IDU,IDS,C) :: (solucoes(IDS, cuidado(IDS,_,_,_), S),
                      comprimento(S, L), 
                      L == 1).	

% Invariante Estrutural: garantir a remoção de conhecimento

-ato(D,IDU,IDS,C) :: (solucoes(ID, cuidado(ID, _, _, _), S),
                      comprimento(S, L), 
                      L == 0).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaUtentes
% Opcao, Parametro, Lista -> {V,F}

listaUtentes( id,P,L ) :- findall(utente(P,X,Y,Z), utente(P,X,Y,Z), L).
listaUtentes( nome,P,L ) :- findall(utente(X,P,Y,Z), utente(X,P,Y,Z), L).
listaUtentes( idade,P,L ) :- findall(utente(X,Y,P,Z), utente(X,Y,P,Z), L).
listaUtentes( morada,P,L ) :- findall(utente(X,Y,Z,P), utente(X,Y,Z,P), L).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaInst
% Lista -> {V,F}

listaInst(L) :- findall(N, cuidado(_,_,N,_), L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que remove os repetidos de uma lista
%Lista -> {V,F}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes

solucoes(X,Y,Z) :- findall(X,Y,Z).

%---------------------------------- - - - - - - - - - -  -  -  -  -  -
% Predicado «comprimento» que calcula o número de elementos
% existentes numa lista

comprimento(L,S) :- length(L,S). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :- (solucoes(INV, +Termo::INV, LINV),
                      insercao(Termo),
                      testa(LINV)).
insercao( Termo ) :-  assert( Termo ).
insercao( Termo ) :-  retract( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a redução do conhecimento

involucao( Termo ) :- (solucoes(INV, -Termo::INV, LINV),
                       remocao(Termo),
                       testa(LINV)).

remocao( Termo ) :- retract( Termo ). 
remocao( Termo ) :- assert( Termo ), !, fail.

%--------------------------------------------------------------------
% Testa

testa([]).
testa([H|T]) :- H,testa(T).