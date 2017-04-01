%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Resolução do Exercício prático 2
t
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic ato/3.
:- dynamic instituicao/2.
:- dynamic cuidado/4.
:- dynamic '-'/1.




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dados da base de conhecimento
utente( 1,diogo,21,braga ).
utente( 2,rui,20,braga ).
utente( 3,esm,21,prado ).
utente( 4,miguel,22,viana ).
utente( 5,joao,26,guimaraes ).
utente( 6,lisandra,25,fafe ).
utente( 7,paulo,24,braganca ).

-utente( 20,anastacia,30,felgueiras ).
-utente( 21,manuel,20,braganca ).

utente( 22,maria,xpto001,guarda ).
excecao( utente( IDU,N,I,M ) ) :-
    utente( IDU,N,xpto001,M ).

excecao( utente( 23,joana,22,braga ) ).
excecao( utente( 23,joana,22,guimaraes ) ).
excecao( utente( 24,mauricio,I,lisboa ) ) :-
    I >= 18, I =< 24.

utente( 25,trump,70,xpto006 ).
excecao( utente( IDU,N,I,L ) ) :-
    utente( IDU,N,I,xpto006 ).
nulo( xpto006 ).
+utente( IDU,N,I,M ) :: ( solucoes( X,( utente( 25,_,_,X ),nao( nulo( X ) ) ),S ),
                          comprimento( S,N ),
                          N == 0 ).



instituicao( hpbraga,braga ).
instituicao( hsjoao,porto ).
instituicao( hviana,viana ).
instituicao( hporto,porto ).
instituicao( hfaro,faro ).

-instituicao( hsjoao,braga ).

instituicao( hsmaria,xpto002 ).
excecao( instituicao( I,L ) ) :-
    instituicao( I,xpto002 ).



cuidado( 1,analises,instituicao( hpbraga,braga ) ).
cuidado( 2,tac,instituicao( hsjoao,porto ) ).
cuidado( 3,nascimento,instituicao( hpbraga,braga ) ).
cuidado( 4,febre,instituicao( hviana,viana ) ).
cuidado( 5,dar-sangue,instituicao( hpbraga,braga ) ).
cuidado( 6,raioX,instituicao( hporto,porto ) ).
cuidado( 7,consulta,instituicao( hporto,porto ) ).
cuidado( 8,nascimento,instituicao( hfaro,faro ) ).
cuidado( 9,ecografia,instituicao( hfaro,faro ) ).
cuidado( 10,quimioterapia,instituicao( hsjoao,porto ) ).

-cuidado( ID,ortopedia,instituicao( hfaro,faro ) ).

cuidado( 20,pediatria,xpto003 ).
excecao( cuidado( IdServ,D,I ) ) :-
    cuidado( IdServ,D,xpto003 ).

excecao( cuidado( 21,oftalmologia,instituicao( hsjoao,porto ) ) ).
excecao( cuidado( 21,oftalmologia,instituicao( hsporto,porto ) ) ).



ato( data( 1,2,1996 ),3,3,10 ).
ato( data( 15,3,2017 ),1,2,15 ).
ato( data( 17,4,1997 ),4,4,5 ).
ato( data( 15,3,2007 ),1,5,0 ).
ato( data( 15,3,2007 ),2,5,0 ).
ato( data( 15,3,2007 ),3,2,0 ).
ato( data( 16,3,2017 ),5,6,12 ).
ato( data( 16,3,2007 ),6,9,20 ).
ato( data( 16,3,2007 ),3,1,40 ).

-ato( data( D,M,2005 ),1,1,P ).
-ato( data( 31,8,2000 )3,7,50 ).

ato( xpto004,6,2,80 ).
excecao( ato( D,IDU,IdServ,P ) ) :-
    ato( xpto004,IDU,IdServ,P ).

ato( data( xpto005,3,2007 ),5,3,20 ).
excecao( ato( data( D,M,A ),IDU,IdServ,P ) ) :-
    ato( data( xpto005,M,A ),IDU,IdServ,P ).

excecao( ato( data(3,6,2007 ),6,9,P ) ) :-
    P >= 10, P =< 25.

ato( data(1,4,2017),25,10,xpto007 ).
excecao( ato( D,IDU,IdServ,P ) ) :-
    ato( D,IDU,IdServ,xpto007 ).
nulo( xpto007 ).
+ato( D,IDU,IdServ,P ) :: ( solucoes( P,( ato( _,25,_,P ),nao( nulo( P ) ) ),S ),
                            comprimento( S,N ),
                            N == 0 ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao dos predicados dia, mes e ano: Data, Dia/Mes/Ano -> {V,F}

dia( data( D,M,A ),D ).
mes( data( D,M,A ),M ).
ano( data( D,M,A ),A ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+utente( ID,N,I,M ) :: ( solucoes( ID, utente( ID,_,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 1 ).


% Invariante Referencial: garantir a consistênica de conhecimento

% O Utente removido não pode ter atos associados
-utente( ID,N,I,M ) :: ( solucoes( ID,ato( _,ID,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 0 ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado prestado: 
% IdServ, Descrição, Instituição, Cidade -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+cuidado( ID,D,I,C ) :: ( solucoes( ID,cuidado( ID,_,_,_ ),S ),
                          comprimento( S,L ), 
                          instituicao( I ),
                          L == 1 ).

% Invariante Referencial: garantir a consistênica de conhecimento

% O Cuidado removido não pode ter atos medicos associados
-cuidado( ID,D,I,C ) :: ( solucoes( ID,ato( _,_,ID,_ ),S ),
                          comprimento( S,L ),
                          L == 0 ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado Instituição: 
% Nome -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+instituicao( I ) :: ( solucoes( I,instituicao( I ),S ),
                          comprimento( S,L ), 
                          L == 1 ).

% Invariante Referencial: garantir a consistênica de conhecimento

-instituicao( I ) :: ( solucoes( I,cuidado(_,_,I,_),S ),
                          comprimento( S,L ), 
                          L == 0 ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: 
% Data, IdUtente, IdServico, Custo -> {V,F}

% Invariante Estrutural: nao permitir a insercao de conhecimento
%                        repetido

+ato( D,IDU,IDS,C ) :: ( solucoes( IDU,utente( IDU,_,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 1 ).	
+ato( D,IDU,IDS,C ) :: ( solucoes( IDS,cuidado( IDS,_,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 1 ).







%------------------------ Predicados Auxiliares -------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado demoLista: Lista1, R -> {V,F}

demoLista( [],[] ).
demoLista( [Q|L],[R|S] ) :-
    demo( Q,R ),
    demoLista( L,S ).

demo( Q,verdadeiro ) :-
    Q.
demo( Q,falso ) :-
    -Q.
demo( Q,desconhecido ) :-
    nao( Q ),
    nao( -Q ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao,
    !, fail.

nao( Questao ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concat: Lista1, Lista2, R -> {V,F}

concat( [],L,L ).
concat( [X|Xs],L2,[X|L] ) :-
    concat( Xs,L2,L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado tiraRepetidos: Lista, Resultado -> {V,F}

tiraRepetidos([],[]).
tiraRepetidos([H|Lista],Res) :-
	eliminaElementos(H,Lista,R),
	concat([H],Rn,Res),
	tiraRepetidos(R,Rn).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado eliminaElementos: Elemento, Lista, Resultado -> {V,F}

eliminaElementos(X,[],[]).
eliminaElementos(X,[X|T],R) :-
	eliminaElementos(X,T,R).
eliminaElementos(X,[H|T],[H|Z]) :-
	X \== H,
	eliminaElementos(X,T,Z).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elem, Lista -> {V,F}

pertence( X,[X|T] ).
pertence( X,[H|T] ) :-
    pertence( X,T ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).




%---------------------------------- - - - - - - - - - -  -  -  -  -  -
% Predicado «comprimento» que calcula o número de elementos
% existentes numa lista

comprimento( L,S ) :-
    length( L,S ). 




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução do conhecimento

evolucao( Termo ) :-
    solucoes( INV,+Termo::INV,LINV ),
    insercao( Termo ),
    testa( LINV ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a inserção de conhecimento

insercao( Termo ) :- 
    assert( Termo ).
insercao( Termo ) :- 
    retract( Termo ),
    !, fail.




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involução do conhecimento

involucao( Termo ) :-
    solucoes( INV,-Termo::INV,LINV ),
    Termo,
    remocao( Termo ),
    testa( LINV ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a remoção de conhecimento

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),
    !, fail.




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que testa uma lista de termos

testa( [] ).
testa( [H|T] ) :-
    H,
    testa(T).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que calcula a soma de um conjunto de valores

somatorio([],0).
somatorio([X|Y],N):- somatorio(Y,R), N is R+X. 




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que calcula o último elemento de uma lista

ultimo([X|Xs], Last) :-  lastAux(Xs, X, Last).

lastAux([], Last, Last).
lastAux([X|Xs], _, Last) :- lastAux(Xs, X, Last).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maximo: Lista, Resultado -> {V,F}
% Resultado é o maximo da lista de tuplos

tuploMaximo( [(X,Y)], (X,Y) ).
tuploMaximo( [(X,Y)|T], (X,Y)) :- 
    tuploMaximo(T, (XX,YY)),
    Y > YY.
tuploMaximo( [(X,Y)|T], (XX,YY)) :-
    tuploMaximo(T, (XX,YY)),
    Y =< YY.