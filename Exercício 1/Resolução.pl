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
:- dynamic cliente/4.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cliente: IdUt, Nome, Idade, Morada -> {V,F}
%                       cliente: IdUt -> {V,F}

cliente(1,diogo,20,braga).
cliente(Id) :- cliente(Id,N,I,M).

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+cliente(ID, N, I, M) :: (solucoes(ID, cliente(ID, NN, II, MM), S),
                          comprimento(S, L), 
                          L == 1).

% Invariante Estrutural: garantir a remoção de conhecimento

-cliente(ID, N, I, M) :: (solucoes(ID, cliente(ID, NN, II, MM), S),
                          comprimento(S, L), 
                          L == 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado prestado: 
% IdServ, Descrição, Instituição, Cidade -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+cuidado(ID,D,I,C) :: (solucoes(ID, (cuidado(ID, D, I, C)), S),
                       comprimento(S, L), 
                       L == 1).

% Invariante Estrutural: garantir a remoção de conhecimento

-cuidado(ID,D,I,C) :: (solucoes(ID, cuidado(ID, D, I, C), S),
                       comprimento(S, L), 
                       L == 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: 
% Data, IdUtente, IdServico, Custo -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+ato(D,IDU,IDS,C) :: (solucoes(ID, cuidado(ID, D, I, C), S),
                      comprimento(S, L), 
                      L == 1).

% Invariante Estrutural: garantir a remoção de conhecimento

-ato(D,IDU,IDS,C) :: (solucoes(ID, cuidado(ID, D, I, C), S),
                      comprimento(S, L), 
                      L == 0).

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
                      assert(Termo),
                      testa(LINV)).
evolucao( Termo ) :- retract(Termo).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a redução do conhecimento

reducao( Termo ) :- (solucoes(INV, -Termo::INV, LINV),
                     retract(Termo),
                     testa(LINV)).
reducao( Termo ) :- assert(Termo).

%--------------------------------------------------------------------
% Testa

testa([]).
testa([H|T]) :- H,testa(T).