%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Resolução do Exercício prático 2
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- op(500,yfx,'and' ).
:- op(400,yfx,'or' ).
:- op(300,yfx,'xor' ).
:- op(600,yfx,'=>' ).
:- op(700,yfx,'<=>' ).
:- dynamic utente/4.
:- dynamic ato/5.
:- dynamic instituicao/2.
:- dynamic cuidado/3.
:- dynamic nulo/1.
:- dynamic excecao/1.
:- dynamic '-'/1.
:- dynamic '::'/2.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F,D}

% ------- Conhecimento Perfeito Positivo -------%

utente( 1,diogo,21,braga ).
utente( 2,rui,20,braga ).
utente( 3,esm,21,prado ).
utente( 4,miguel,22,viana ).
utente( 5,joao,26,guimaraes ).
utente( 6,lisandra,25,fafe ).
utente( 7,paulo,24,braganca ).


% ------- Conhecimento Perfeito Negativo -------%

%%  O utente manuel com id 21, 20 anos, nega ser de bragança. 

-utente( 21,manuel,20,braganca ).

%%  O utente com o id 20, chamada anastacia de felgueiras nega ter 30 anos.

-utente( 20,anastacia,30,felgueiras ).


% ------- Conhecimento Imperfeito Incerto ------%

%% Desconhece-se a idade do utente com o IdUt 22,
%% de nome maria e residente na guarda.

utente( 22,maria,xpto001,guarda ).
excecao( utente( IDU,N,I,M ) ) :-
    utente( IDU,N,xpto001,M ).


% ------- Conhecimento Imperfeito Impreciso ------%

%%  A joana, utente com IdUt 23 e com 22 anos de idade,
%%  mora em braga ou em guimaraes.

excecao( utente( 23,joana,22,braga ) ).
excecao( utente( 23,joana,22,guimaraes ) ).

%%  Não se sabe ao certo a  idade do mauricio, com o IdUt 24
%%  e residente em lisboa. Apenas se sabe que possui entre 18 a 24 anos .

excecao( utente( 24,mauricio,I,lisboa ) ) :-
    I >= 18, I =< 24.


% -------  Conhecimento Imprefeito Interdito ------%

%%  O utente com o IdUt 25, de nome trump, com 70 anos,
%%  exige que não se saiba a sua morada.

utente( 25,trump,70,xpto006 ).
excecao( utente( IDU,N,I,L ) ) :-
    utente( IDU,N,I,xpto006 ).
nulo( xpto006 ).
+utente( IDU,N,I,M ) :: ( solucoes( X,( utente( 25,_,_,X ),nao( nulo( X ) ) ),S ),
                          comprimento( S,L ),
                          L == 0 ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-utente( IDU,N,I,M ) :-
    nao( utente( IDU,N,I,M ) ),
    nao( excecao( utente( IDU,N,I,M ) ) ).


% Não permitir a insercao de conhecimento repetido

+utente( ID,N,I,M ) :: ( solucoes( ID, utente( ID,_,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 1 ).


% Garantir a consistênica de conhecimento: o Utente removido não pode ter atos associados

-utente( ID,N,I,M ) :: ( solucoes( ID,ato( _,_,ID,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 0 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado prestado: 
% IdServ, Descrição, Instituição -> {V,F,D}

% ------- Conhecimento Perfeito Positivo -------%

cuidado( 1,analises,instituicao( hpbraga,braga ) ).
cuidado( 2,tac,instituicao( hsjoao,porto ) ).
cuidado( 3,parto,instituicao( hpbraga,braga ) ).
cuidado( 4,ortopedia,instituicao( hviana,viana ) ).
cuidado( 5,doar-sangue,instituicao( hpbraga,braga ) ).
cuidado( 6,raioX,instituicao( hporto,porto ) ).
cuidado( 7,medicina-geral,instituicao( hporto,porto ) ).
cuidado( 8,parto,instituicao( hfaro,faro ) ).
cuidado( 9,ecografia,instituicao( hfaro,faro ) ).
cuidado( 10,quimioterapia,instituicao( hsjoao,porto ) ).


% ------- Conhecimento Perfeito Negativo -------%

%%  A instituição hfaro nao presta o cuidado ortopedia.

-cuidado( ID,ortopedia,instituicao( hfaro,faro ) ).


% Conhecimento Imperfeito Incerto

cuidado( 20,pediatria,xpto003 ).
excecao( cuidado( IdS,D,I ) ) :-
    cuidado( IdS,D,xpto003 ).


% Conhecimento Imperfeito Impreciso

excecao( cuidado( 21,oftalmologia,instituicao( hsjoao,porto ) ) ).
excecao( cuidado( 21,oftalmologia,instituicao( hsporto,porto ) ) ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-cuidado( IdS,D,I ) :-
    nao( cuidado( IdS,D,I ) ),
    nao( excecao( cuidado( IdS,D,I ) ) ).


% Não permitir a insercao de cuidados com o mesmo identificador

+cuidado( ID,D,I ) :: ( solucoes( ID,cuidado( ID,_,_ ),S ),
                        comprimento( S,L ), 
                        L == 1 ).


% Não permitir a insercao de cuidados repetidos numa instituição

+cuidado( ID,D,I ) :: ( solucoes( D,cuidado( _,D,I ),S ),
                        comprimento( S,L ), 
                        L == 1 ).


% A instituição deve exisir na base de conhecimento

+cuidado( ID,D,instituicao( I,L ) ) :: instituicao( I,L ).


% O Cuidado removido não pode ter atos medicos associados

-cuidado( ID,D,I ) :: ( solucoes( ID,ato( _,_,_,ID,_ ),S ),
                        comprimento( S,L ),
                        L == 0 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado Instituição: Nome, Cidade -> {V,F,D}

% Conhecimento Perfeito Positivo

instituicao( hpbraga,braga ).
instituicao( hsjoao,porto ).
instituicao( hviana,viana ).
instituicao( hporto,porto ).
instituicao( hfaro,faro ).


% Conhecimento Perfeito Negativo

-instituicao( hsjoao,braga ).


% Conhecimento Imperfeito Incerto

instituicao( hsmaria,xpto002 ).
excecao( instituicao( I,L ) ) :-
    instituicao( I,xpto002 ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-instituicao( N,C ) :- nao( instituicao( N,C ) ), 
                       nao( excecao( instituicao( N,C ))).


% Não permitir a insercao de instituição com o mesmo nome

+instituicao( I,C ) :: ( solucoes( I,instituicao( I,_ ),S ),
                         comprimento( S,L ), 
                         L == 1 ).


% Não permite remoção se estiver associado algum cuidado prestado

-instituicao( I,C ) :: ( solucoes( I,cuidado(_,_,I,_),S ),
                         comprimento( S,L ), 
                         L == 0 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: 
% Data, IdUtente, IdServico, Custo -> {V,F,D}

% Conhecimento Perfeito Positivo

ato( 1,data( 1,2,1996 ),3,3,10 ).
ato( 2,data( 15,3,2017 ),1,2,15 ).
ato( 3,data( 17,4,1997 ),4,4,5 ).
ato( 4,data( 15,3,2007 ),1,5,0 ).
ato( 5,data( 15,3,2007 ),2,5,0 ).
ato( 6,data( 15,3,2007 ),3,2,0 ).
ato( 7,data( 16,3,2017 ),5,6,12 ).
ato( 8,data( 16,3,2007 ),6,9,20 ).
ato( 9,data( 16,3,2007 ),3,1,40 ).


% Conhecimento Perfeito Negativo

-ato( 20,data( D,M,2005 ),1,1,P ).
-ato( 21,data( 31,8,2000 ),3,7,50 ).


% Conhecimento Imperfeito Incerto

ato( 22,xpto004,6,2,80 ).
excecao( ato( ID,D,IDU,IdS,P ) ) :-
    ato( ID,xpto004,IDU,IdS,P ).

ato( 23,data( xpto005,3,2007 ),5,3,20 ).
excecao( ato( ID,data( D,M,A ),IDU,IdS,P ) ) :-
    ato( ID,data( xpto005,M,A ),IDU,IdS,P ).


% Conhecimento Imperfeito Impreciso

excecao( ato( 24,data(3,6,2007 ),6,9,P ) ) :-
    P >= 10, P =< 25.


% Conhecimento Imperfeito Interdito

ato( d25,ata(1,4,2017),25,10,xpto007 ).
excecao( ato( ID,D,IDU,IdS,P ) ) :-
    ato( ID,D,IDU,IdS,xpto007 ).
nulo( xpto007 ).

+ato( ID,D,IDU,IdS,P ) :: ( solucoes( P,( ato( _,_,25,_,P ),nao( nulo( P ) ) ),S ),
                            comprimento( S,N ),
                            N == 0 ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-ato( ID,D,IDU,IDS,C ) :- nao( ato( ID,D,IDU,IDS, C )),
                          nao( excecao( ato( ID,D,IDU,IDS, C ) )).


% Não permitir a insercao de conhecimento repetido

-ato( ID,D,IDU,IDS,C ) :: ( solucoes( ID, ato( ID,_,_,_,_ ),S ),
                            comprimento( S,L ), 
                            L == 1 ).


% Não permitir inserir atos com IdUt não registados

+ato( ID,D,IDU,IDS,C ) :: ( solucoes( IDU,utente( IDU,_,_,_ ),S ),
                            comprimento( S,L ), 
                            L == 1 ).


% Não permitir inserir atos com IdServ não registados

+ato( ID,D,IDU,IDS,C ) :: ( solucoes( IDS,cuidado( IDS,_,_,_ ),S ),
                            comprimento( S,L ), 
                            L == 1 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao dos predicados dia, mes e ano: Data, Dia/Mes/Ano -> {V,F}

dia( data( D,M,A ),D ).
mes( data( D,M,A ),M ).
ano( data( D,M,A ),A ).



%------------------------ Predicados Auxiliares -------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado demo: Questao, Resposta -> {V,F}

demo( Q,verdadeiro ) :-
    Q.
demo( Q,falso ) :-
    -Q.
demo( Q,desconhecido ) :-
    nao( Q ),
    nao( -Q ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado demo2: (Questao1 Questao2), Resposta -> {V,F}

demo2( (Q1 and Q2),verdadeiro ) :-
    demo( Q1,verdadeiro ),
    demo( Q2,verdadeiro ).
demo2( (Q1 and Q2),falso ) :-
    demo( Q1,falso ).
demo2( (Q1 and Q2),falso ) :-
    demo( Q2,falso ).
demo2( (Q1 and Q2),desconhecido ).

demo2( (Q1 or Q2),verdadeiro ) :-
    demo( Q1,verdadeiro ).
demo2( (Q1 or Q2),verdadeiro ) :-
    demo( Q2,verdadeiro ).
demo2( (Q1 or Q2),falso ) :-
    demo( Q1,falso ),
    demo( Q2,falso ).
demo2( (Q1 or Q2),desconhecido ).

demo2( (Q1 xor Q2),falso ) :-
    demo( Q1,falso ),
    demo( Q2,falso ).
demo2( (Q1 xor Q2),falso ) :-
    demo( Q1,verdadeiro ),
    demo( Q2,verdadeiro ).
demo2( (Q1 xor Q2),verdadeiro ) :-
    demo( Q1, verdadeiro ),
    demo( Q2, falso ).
demo2( (Q1 xor Q2),verdadeiro ) :-
    demo( Q1, falso ),
    demo( Q2, verdadeiro ).
demo2( (Q1 xor Q2), desconhecido ).

demo2( (Q1 => Q2),falso ) :-
    demo( Q1,verdadeiro ),
    demo(Q2, falso).
demo2( (Q1 => Q2),verdadeiro ) :-
    demo( Q1,falso ).
demo2( (Q1 => Q2),verdadeiro ) :-
    demo( Q2,verdadeiro ).
demo2( (Q1 => Q2),desconhecido ).

demo2( (Q1 <=> Q2),verdadeiro ) :-
    demo( Q1,verdadeiro ),
    demo( Q2,verdadeiro ).
demo2( (Q1 <=> Q2),verdadeiro ) :-
    demo( Q1,falso ),
    demo( Q2,falso ).
demo2( (Q1 <=> Q2),falso ) :-
    demo( Q1,verdadeiro ),
    demo( Q2,falso ).
demo2( (Q1 <=> Q2),falso ) :-
    demo( Q1,falso ),
    demo( Q2,verdadeiro ).
demo2( (Q1 <=> Q2),desconhecido ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado demoLista: Lista, Lista resultado -> {V,F}

demoLista( [],[] ).
demoLista( [Q|Qs],[R|Rs] ) :-
    demo( Q,R ),
    demoLista( Qs,Rs ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado demoListaConj: Lista, Resultado -> {V,F}

demoListaConj( [Q],R ) :- demo( Q,R ).
demoListaConj( [Q|Qs],R ) :-
    demo( Q,R1 ),
    demoListaConj( Qs,R2 ),
    demo2((R1 and R2),R).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado excecao: Termo -> {V,F}

+excecao( T ) :: ( solucoes( T,excecao( T ),S ),
                   comprimento( S,L ), 
                   L == 1 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nulo: Termo -> {V,F}

+nulo( T ) :: ( solucoes( T,nulo( T ),S ),
                comprimento( S,L ), 
                L == 1 ).

-nulo( T ) :: ( solucoes( T,nulo( T ),S ),
                comprimento( S,L ), 
                L == 1 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao,
    !, fail.
nao( Questao ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% perfeito

evolucao( Termo ) :-
    solucoes( INV,+Termo::INV,LINV ),
    insercao( Termo ),
    testa( LINV ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo incerto

evolucaoIncerto( utente( ID,N,I,M ),idade ) :-
    nao( existeXPTO( I ) ),
    evolucao( utente( ID,N,I,M ) ),
    assert( ( excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,I,MU ) ) ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo impreciso

evolucaoImpreciso( Termo ) :-
    evolucao( excecao( Termo ) ).
evolucaoImpreciso( utente( ID,N,I,M ),idade,Linf,Lsup ) :-
    assert( ( excecao( utente( ID,N,I,M ) ) :- I >= Linf, I =< Lsup ) ).
evolucaoImpreciso( ato( ID,D,IDU,IDS,C ),custo,Linf,Lsup ) :-
    assert( ( excecao( ato( ID,D,IDU,IDS,C ) ) :- C >= Linf, I =< Lsup ) ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo interdito

evolucaoInterdito( utente( ID,N,I,M ),idade ) :-
    nao( existeXPTO( I ) ),
    evolucao( utente( ID,N,I,M ) ),
    assert( ( excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,I,MU ) ) ),
    assert( nulo( I ) ),
    assert( ( +utente( IDU,NU,IU,MU ) :: ( solucoes( X,( utente( ID,_,X,_ ),nao( nulo( X ) ) ),S ),
                                           comprimento( S,L ),
                                           L == 0 ) ) ).


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

existeXPTO( XPTO ) :-
    solucoes( XPTO,utente( _,XPTO,_,_ ),S1 ),
    solucoes( XPTO,utente( _,_,XPTO,_ ),S2 ),
    solucoes( XPTO,utente( _,_,_,XPTO ),S3 ),
    comprimento( S1,L1 ),
    comprimento( S2,L2 ),
    comprimento( S3,L3 ),
    L2 > 0.



%---------------------------------- - - - - - - - - - -  -  -  -  -  -
% Predicado «comprimento» que calcula o número de elementos
% existentes numa lista

comprimento( L,S ) :-
    length( L,S ). 