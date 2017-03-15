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
cuidado(4,febre, viana, viana).
cuidado(5, dar-sangue, hpbraga, braga).

ato(01-02-1996, 3,3,10).
ato(15-03-2017, 1,2,15).
ato(14-03-2014, 4,4,5).
ato(15-03-2017, 1,5,0).
ato(15-03-2017, 2,5,0).

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
% Extensao do predicado pesquisaUtentes
% Opcao, Parametro, Lista -> {V,F}

pesquisaUtentes( id,P,L ) :- findall(utente(P,X,Y,Z), utente(P,X,Y,Z), L).
pesquisaUtentes( nome,P,L ) :- findall(utente(X,P,Y,Z), utente(X,P,Y,Z), L).
pesquisaUtentes( idade,P,L ) :- findall(utente(X,Y,P,Z), utente(X,Y,P,Z), L).
pesquisaUtentes( morada,P,L ) :- findall(utente(X,Y,Z,P), utente(X,Y,Z,P), L).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaInst
% Lista -> {V,F}

listaInst(S) :- findall(N, cuidado(_,_,N,_), L),
                tiraRepetidos( L, S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaCui
% Opcao, Parametro, Lista -> {V,F}

listaCui( cidade, C, L) :- findall(cuidado(X,Y,Z,C), cuidado(X,Y,Z,C), L).
listaCui( inst, I, L) :- findall(cuidado(X,Y,I,Z), cuidado(X,Y,I,Z), L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaUtentes que lista os utentes por 
% uma Instituição/cidade
% Opcao, Parametro, Lista -> {V,F}

listaUtentes( inst, I, L) :- procuraCui( inst, I, Temp),
                             procuraAtos(Temp, Temp2),
                             procuraUtentes(Temp2, L).

listaUtentes( cuid, C, L) :- procuraCui(cuid, C, Temp),
                             procuraAtos(Temp, Temp2),
                             procuraUtentes(Temp2, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraCui que devolde uma lista com os
% ids dos cuidados, procurando pela Descrição do cuidado ou a Instituição
% do servico. 
% Tipo, Parametro, Lista -> {V,F}

procuraCui( inst, I, L) :- findall(X, cuidado(X,_,I,_), Temp),
                      tiraRepetidos(Temp, L).

procuraCui( cuid, D, L) :- findall(X, cuidado(X,D,_,_), Temp),
                      tiraRepetidos(Temp, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraAtos
% Lista servicos, Lista -> {V,F}

procuraAtos( [X], L) :- findall(U, ato(_,U,X,_), L).

procuraAtos( [H|T], L ) :- findall(U, ato(_,U,H,_), Temp),
                           procuraAtos(T,Temp2),
                           concat(Temp,Temp2, Temp3),
                           tiraRepetidos(Temp3,L).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraUtentes
% Lista de ids de utentes, Lista -> {V,F}

procuraUtentes( [X], L) :- findall((X,N), utente(X,N,_,_), L).

procuraUtentes( [H|T], L ) :- findall((H,N), utente(H,N,_,_), Temp),
                              procuraUtentes(T,Temp2),
                              concat(Temp,Temp2, Temp3),
                              tiraRepetidos(Temp3, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaAtosMed que lista os atos medicos por 
% uma utente/ Instituição /cidade
% Opcao, Parametro, Lista -> {V,F}

listaAtosMed( utente, IDU, L) :- findall(ato(D,IDU,IDC,C), ato(D,IDU,IDC,C), Temp),
                                 tiraRepetidos(Temp, L).

listaAtosMed( cuid, IDC, L) :- findall(ato(D,IDU,IDC,C), ato(D,IDU,IDC,C), Temp),
                               tiraRepetidos(Temp, L).

listaAtosMed( inst, I, L) :-  procuraCui(inst, I,Temp),
                              listarAtos(Temp, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarAtos
% ids servicos, Lista -> {V,F}

listarAtos( [ID], L) :- findall(ato(X,Y,ID,Z), ato(X,Y,ID,Z), L).

listarAtos( [ID|T], L ) :- findall(ato(X,Y,ID,Z), ato(X,Y,ID,Z), Temp),
                           listarAtos(T,Temp2),
                           concat(Temp,Temp2, Temp3),
                           tiraRepetidos(Temp3,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado histUtente
% Tipo, id Utente, Lista -> {V,F}

histUtente( inst, ID, L) :- atosCuidados(ID, Temp),
                            cuidadosInst(Temp, L).

histUtente( cuid, ID, L) :- findall(IDC, ato(_,ID, IDC,_), Temp),
                            tiraRepetidos(Temp, L).                

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atosCuidados que devolve a lista com os cuidados de
%um utente, a partir do seu id. 
% id Utente, Lista -> {V,F}

atosCuidados(ID, L) :- findall(IDC, ato(_,ID, IDC,_), Temp),
                       tiraRepetidos( Temp, L).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidadosInst que devolve a lista com as instituicoes
% de uma lista de cuidados. 
% Lista ids cuidado, Lista -> {V,F}

cuidadosInst([IDC], L) :- findall(I, cuidado(IDC,_,I,_), L).

cuidadosInst([IDC|T], L) :- findall(I, cuidado(IDC,_,I,_), Temp),
                            cuidadosInst(T,Temp2),
                            concat(Temp, Temp2, Temp3),
                            tiraRepetidos(Temp3, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concat, que junta duas listas
% Lista1, Lista2, R -> {V,F}

concat([],L,L).
concat([X|Xs], L2, [X|L]) :- concat(Xs,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do predicado que remove os repetidos de uma lista
%Lista -> {V,F}

tiraRepetidos([],[]).
tiraRepetidos([H|T], [H|R]) :- nao( pertence(H,T)), 
                               tiraRepetidos(T,R).
tiraRepetidos([H|T], [R]) :-   pertence(H,T), 
                               tiraRepetidos(T,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao
% Questao  -> {V,F}

nao( Questao ) :- Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que determina se um elemnto pertence a uma lista
%Elem, Lista -> {V,F}

pertence(X, [X|T]).
pertence(X, [H|T]) :- pertence(X,T).

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