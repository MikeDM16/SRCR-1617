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
% Extensao do predicado cliente: IdUt, Nome, Idade, Morada -> {V,F,D}

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+cliente(Id, N, I, M) :: (solucoes((P, F), (pai(P, F)), S),
               comprimento(S, N), 
			   N == 1).