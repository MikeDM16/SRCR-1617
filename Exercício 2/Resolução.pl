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
:- dynamic counter_idu/1.
:- dynamic counter_ids/1.
:- dynamic counter_ida/1.
:- dynamic counter_xpto/1.

counter_idu(13).
counter_ids(14).
counter_ida(16).
counter_xpto(48).


increment( idu ) :-
    retract( counter_idu( C ) ),
    C1 is C + 1,
    assert( counter_idu( C1 ) ) .
increment( ids ) :-
    retract( counter_ids( C ) ),
    C1 is C + 1,
    assert( counter_ids( C1 ) ) .
increment( ida ) :-
    retract( counter_ida( C ) ),
    C1 is C + 1,
    assert( counter_ida( C1 ) ) .
increment( xpto ) :-
    retract( counter_xpto( C ) ),
    C1 is C + 1,
    assert( counter_xpto( C1 ) ) .


getIncXPTO( XPTO ) :-
    increment( xpto ),
    counter_xpto( XPTO2 ),
    name( XPTO,[120,112,116,111,XPTO2] ).

getIncIDU( IDU ) :-
    increment( idu ),
    counter_idu( IDU ).

getIncIDS( IDS ) :-
    increment( ids ),
    counter_ids( IDS ).

getIncIDA( IDA ) :-
    increment( ida ),
    counter_ida( IDA ).



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

%%  O utente manuel com id 8, 20 anos, nega ser de bragança. 

-utente( 8,manuel,20,braganca ).

%%  O utente com o id 9, chamada anastacia de felgueiras nega ter 30 anos.

-utente( 9,anastacia,30,felgueiras ).


% ------- Conhecimento Imperfeito Incerto ------%

%% Desconhece-se a idade do utente com o IdUt 10,
%% de nome maria e residente na guarda.

utente( 10 ,maria,xpto001,guarda ).
excecao( utente( IDU,N,I,M ) ) :-
    utente( IDU,N,xpto001,M ).


% ------- Conhecimento Imperfeito Impreciso ------%

%%  A joana, utente com IdUt 11 e com 22 anos de idade,
%%  mora em braga ou em guimaraes.

excecao( utente( 11,joana,22,braga ) ).
excecao( utente( 11,joana,22,guimaraes ) ).

%%  Não se sabe ao certo a  idade do mauricio, com o IdUt 12
%%  e residente em lisboa. Apenas se sabe que possui entre 18 a 24 anos .

excecao( utente( 12,mauricio,I,lisboa ) ) :-
    I >= 18, I =< 24.


% -------  Conhecimento Imperfeito Interdito ------%

%%  O utente com o IdUt 13, de nome trump, com 70 anos,
%%  exige que não se saiba a sua morada.

utente( 13,trump,70,xpto002 ).
excecao( utente( IDU,N,I,L ) ) :-
    utente( IDU,N,I,xpto002 ).
nulo( xpto002 ).
+utente( IDU,N,I,M ) :: ( solucoes( X,( utente( 13,_,_,X ),nao( nulo( X ) ) ),S ),
                          comprimento( S,L ),
                          L == 0 ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-utente( IDU,N,I,M ) :-
    nao( utente( IDU,N,I,M ) ),
    nao( excecao( utente( IDU,N,I,M ) ) ).


% O Utente a remover não pode ter atos associados

-utente( ID,N,I,M ) :: ( solucoes( ID,ato( _,_,ID,_,_ ),S ),
                         comprimento( S,L ), 
                         L == 0 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado:
% IdServ, Descrição, Instituicao -> {V,F,D}

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

-cuidado( 11,ortopedia,instituicao( hfaro,faro ) ).


% -------  Conhecimento Imperfeito Incerto ------%

%%  É desconhecido o nome da instituição que presta o serviço
%%  com o IdServ 12 , designado por pediatria.

cuidado( 12,pediatria,xpto003 ).
excecao( cuidado( IdS,D,I ) ) :-
    cuidado( IdS,D,xpto003 ).

%%  É desconhecida a descricão do cuidado com o IdServ 12 ,
%%  prestado no hsjoao no porto.

cuidado( 13,xpto004,instituicao( hsjoao,porto )).
excecao( cuidado( IdS,D,I ) ) :-
    cuidado( IdS,xpto004,I ).


% -------  Conhecimento Imperfeito Impreciso ------%

%%  O cuidado com o IdServ 14, designado por oftalmologia,
%%  é prestado em uma das seguintes instituições: hsjoao ou hporto.

excecao( cuidado( 14,oftalmologia,instituicao( hsjoao,porto ) ) ).
excecao( cuidado( 14,oftalmologia,instituicao( hporto,porto ) ) ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-cuidado( IdS,D,I ) :-
    nao( cuidado( IdS,D,I ) ),
    nao( excecao( cuidado( IdS,D,I ) ) ).


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

% ------- Conhecimento Perfeito Positivo -------%

instituicao( hpbraga,braga ).
instituicao( hsjoao,porto ).
instituicao( hviana,viana ).
instituicao( hporto,porto ).
instituicao( hfaro,faro ).


% ------- Conhecimento Perfeito Negativo -------%

%% Em braga nao existe nenhuma instituicao com o nome hsjoao.

-instituicao( hsjoao,braga ).


% -------  Conhecimento Imperfeito Incerto ------%

%% Desconhece-se a localidade do hsmaria.

instituicao( hsmaria,xpto005 ).
excecao( instituicao( I,L ) ) :-
    instituicao( I,xpto005 ).


% -------  Conhecimento Imperfeito Impreciso ------%

%% Não se sabe ao certo se a instituicao hsjose se 
%% encontra localizada em braga ou no porto.

excecao(instituicao(hsjose,braga)).
excecao(instituicao(hsjose,porto)).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-instituicao( N,C ) :- nao( instituicao( N,C ) ), 
                       nao( excecao( instituicao( N,C ))).


% Não permite remoção se estiver associado algum cuidado prestado

-instituicao( I,C ) :: ( solucoes( I,cuidado(_,_,I,_),S ),
                         comprimento( S,L ), 
                         L == 0 ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ato medico: 
% Data, IdUtente, IdServico, Custo -> {V,F,D}

% ------- Conhecimento Perfeito Positivo -------%

ato( 1,data( 1,2,1996 ),3,3,10 ).
ato( 2,data( 15,3,2017 ),1,2,15 ).
ato( 3,data( 17,4,1997 ),4,4,5 ).
ato( 4,data( 15,3,2007 ),1,5,0 ).
ato( 5,data( 15,3,2007 ),2,5,0 ).
ato( 6,data( 15,3,2007 ),3,2,0 ).
ato( 7,data( 16,3,2017 ),5,6,12 ).
ato( 8,data( 16,3,2007 ),6,9,20 ).
ato( 9,data( 16,3,2007 ),3,1,40 ).


% ------- Conhecimento Perfeito Negativo -------%

%%  Sabe-se que o ato com ID 10 ocorrido no dia 31-08-2000,
%%  com IdUt 3 e IDServ 7, não teve um custo de 50 euros.

-ato( 10 ,data( 31,8,2000 ),3,7,50 ).


% ------- Conhecimento Imperfeito Incerto -------%

%%  Desconhece-se a data em que foi prestado o ato medico com IdAto 11, realizado
%%  pelo utente com o IdUt 6, relativo ao cuidado com IdServ 2 com um custo de 80 euros.

ato( 11,xpto006,6,2,80 ).
excecao( ato( ID,D,IDU,IdS,P ) ) :-
    ato( ID,xpto006,IDU,IdS,P ).

%%  Desconhece-se o custo do ato medico com ocorrido em 03-06-2007,
%%  com IDServ 2 e IdUt 6, identificado pelo IDAto 12.

ato( 12,data(3,6,2007 ),6,2,xpto007 ).
excecao( ato( ID,D,IDU,IDS,P ) ) :-
    ato( ID,D,IDU,IDS, xpto007).

%%  Desconhece-se o dia em que foi prestado o cuidado IdServ 3,
%%  pelo utente com o IdUt 5, com um custo de 20 euros. 
%%  Apenas se sabe o ato com IDAto 13 foi prestado em março de 2007.

ato( 13,data( xpto008,3,2007 ),5,3,20 ).
excecao( ato( ID,data( D,M,A ),IDU,IdS,P ) ) :-
    ato( ID,data( xpto008,M,A ),IDU,IdS,P ).


% ------- Conhecimento Imperfeito Impreciso -------%

%%  O ato medico com IdAto 14 ocorrido em 03-06-2007, com o IdUt 6 e IdServ 9 custou entre 10 a 25 euros.

excecao( ato( 14,data(3,6,2007 ),6,9,P ) ) :-
    P >= 10, P =< 25.

%%  O ato medico com IdAto 15 ocorrido em 04-06-2007, com o IdUt 7 e IdServ 1 custou entre 20 ou 30 euros.

excecao( ato( 15,data(4,6,2007 ),6,9,20 ) ).
excecao( ato( 15,data(4,6,2007 ),6,9,30) ).


% ------- Conhecimento Imperfeito Interdito -------%

%%  O utente com o IdUt 13, usufruiu do cuidado medico com o IdServ 10
%%  em 01-04-2017, contudo pretende que não se saiba quanto pagou por este ato identificado pelo IdAto 16.

ato( 16,data(1,4,2017),13,10,xpto009 ).
excecao( ato( ID,D,IDU,IdS,P ) ) :-
    ato( ID,D,IDU,IdS,xpto009 ).
nulo( xpto009 ).

+ato( ID,D,IDU,IdS,P ) :: ( solucoes( P,( ato( 16,_,_,_,P ),nao( nulo( P ) ) ),S ),
                            comprimento( S,N ),
                            N == 0 ).


% Adoção do Pressuposto do Mundo Fechado com consideração de exceções

-ato( ID,D,IDU,IDS,C ) :- nao( ato( ID,D,IDU,IDS, C )),
                          nao( excecao( ato( ID,D,IDU,IDS, C ) )).


% Não permitir inserir atos com IdUt não registados

+ato( ID,D,IDU,IDS,C ) :: ( solucoes( IDU,utente( IDU,_,_,_ ),S ),
                            comprimento( S,L ), 
                            L == 1 ).


% Não permitir inserir atos com IdServ não registados

+ato( ID,D,IDU,IDS,C ) :: ( solucoes( IDS,cuidado( IDS,_,_ ),S ),
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

evolucao( Termo ) :-
    solucoes( INV,+Termo::INV,LINV ),
    insercao( Termo ),
    testa( LINV ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento perfeito
% partindo de conhecimento imperfeito impreciso do utente

evoluirConhecimento( utente( ID,N,I,M )) :-
	demo(utente(ID,N,I,M),desconhecido),
    solucoes( ((excecao( utente( ID,N,X,M) ) :- X >= LI, X =< LS)),(excecao( utente( ID,N,I,M) )),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( utente( ID,N,I,M ) ).

evoluirConhecimento( utente( ID,N,I,M )) :-
	demo(utente(ID,N,I,M),desconhecido),
    solucoes( (excecao( utente( ID,NU,IU,MU ))),(excecao( utente( ID,NU,IU,MU ))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( utente( ID,N,I,M ) ).


% partindo de conhecimento imperfeito impreciso do cuidado

evoluirConhecimento( cuidado( ID,D,I )) :-
	demo(cuidado( ID,D,I ), desconhecido),
    solucoes( (excecao( cuidado( ID,DC,IC ))),(excecao( cuidado( ID,DC,IC ))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( cuidado( ID,D,I )).


% partindo de conhecimento imperfeito impreciso da instituicao

evoluirConhecimento( instituicao( D,I )) :-
	demo(instituicao( D,I ), desconhecido),
    solucoes( (excecao( instituicao( D,II ))),(excecao( instituicao( D,II ))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    assert(instituicao( D,I )).


% partindo de conhecimento imperfeito impreciso do ato

evoluirConhecimento(ato( ID,data( D,M,A ),IDU,IDC,C)) :-
	demo(ato( ID,data( D,M,A ),IDU,IDC,C),desconhecido),
    solucoes( ((excecao( ato( ID,data( X,M,A ),IDU,IDC,C) ) :- X >= LI, X =< LS)),(excecao( ato( ID,data( D,M,A),IDU,IDC,C))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( ato( ID,data( D,M,A ),IDU,IDC,C)).

evoluirConhecimento(ato( ID,data( D,M,A ),IDU,IDC,C)) :-
	demo(ato( ID,data( D,M,A ),IDU,IDC,C),desconhecido),
    solucoes( ((excecao( ato( ID,data( D,X,A ),IDU,IDC,C) ) :- X >= LI, X =< LS)),(excecao( ato( ID,data( D,M,A),IDU,IDC,C))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( ato( ID,data( D,M,A ),IDU,IDC,C)).

evoluirConhecimento(ato( ID,data( D,M,A ),IDU,IDC,C)) :-
	demo(ato( ID,data( D,M,A ),IDU,IDC,C),desconhecido),
    solucoes( ((excecao( ato( ID,data( D,M,X ),IDU,IDC,C) ) :- X >= LI, X =< LS)),(excecao( ato( ID,data( D,M,A),IDU,IDC,C))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( ato( ID,data( D,M,A ),IDU,IDC,C)).

evoluirConhecimento(ato( ID,D,IDU,IDC,C)) :-
	demo(ato( ID,D,IDU,IDC,C),desconhecido),
    solucoes( ((excecao( ato( ID,D,IDU,IDC,X) ) :- X >= LI, X =< LS)),(excecao( ato( ID,D,IDU,IDC,C))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( ato( ID,D,IDU,IDC,C)).

evoluirConhecimento( ato( ID,D,IDU,IDC,C)) :-
	demo(ato( ID,D,IDU,IDC,C), desconhecido),
    solucoes( (excecao( ato( ID,DA,IDA,IDSA,CA))),(excecao( ato( ID,DA,IDA,IDSA,CA))),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    evolucao( ato( ID,D,IDU,IDC,C)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento perfeito
% partindo de conhecimento imperfeito incerto do utente

evoluirConhecimento( utente( ID,N,I,M ) ) :-
	demo(utente(ID,N,I,M),desconhecido),
    solucoes( (excecao( utente( ID,N,I,M ) ) :- utente( ID,X,I,M )),(utente( ID,X,I,M ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeUtente( ID ),
    evolucao( utente( ID,N,I,M ) ).
evoluirConhecimento( utente( ID,N,I,M ) ) :-
    demo(utente(ID,N,I,M),desconhecido),
    solucoes( (excecao( utente( ID,N,I,M ) ) :- utente( ID,N,X,M )),(utente( ID,N,X,M ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeUtente( ID ),
    evolucao( utente( ID,N,I,M ) ).
evoluirConhecimento( utente( ID,N,I,M ) ) :-
    demo(utente(ID,N,I,M),desconhecido),
    solucoes( (excecao( utente( ID,N,I,M ) ) :- utente( ID,N,I,X )),(utente( ID,N,I,X ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeUtente( ID ),
    evolucao( utente( ID,N,I,M ) ).


% partindo de conhecimento imperfeito incerto do cuidado

evoluirConhecimento( cuidado( ID,D,I )) :-
	demo(cuidado( ID,D,I ),desconhecido),
    solucoes( (excecao( cuidado( ID,D,I) ) :- cuidado( ID,X,I)),(cuidado( ID,X,I ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeCuidado( ID ),
    evolucao( cuidado( ID,D,I ) ).
evoluirConhecimento( cuidado( ID,D,I )) :-
	demo(cuidado( ID,D,I ),desconhecido),
    solucoes( (excecao( cuidado( ID,D,I) ) :- cuidado( ID,D,X)),(cuidado( ID,D,X ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeCuidado( ID ),
    evolucao( cuidado( ID,D,I ) ).


% partindo de conhecimento imperfeito incerto da instituicao

evoluirConhecimento( instituicao( D,I)) :-
	demo(instituicao(D,I),desconhecido),
    solucoes( (excecao( instituicao( D,X) ) :- instituicao( D,X)),(instituicao( D,X),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeInst( D ),
    evolucao( instituicao( D,I)).


% partindo de conhecimento imperfeito incerto do ato

evoluirConhecimento( ato( ID,data(D,M,A),IDU,IDS,C ) ) :-
    demo(  ato( ID,data(D,M,A),IDU,IDS,C ),desconhecido),
    solucoes( (excecao(   ato( ID,data(D,M,A),IDU,IDS,C ) ) :- ato( ID,data(X,M,A),IDU,IDS,C )),(  ato( ID,data(X,M,A),IDU,IDS,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao(  ato( ID,data(D,M,A),IDU,IDS,C ) ).

evoluirConhecimento( ato( ID,data(D,M,A),IDU,IDS,C ) ) :-
    demo(  ato( ID,data(D,M,A),IDU,IDS,C ),desconhecido),
    solucoes( (excecao(   ato( ID,data(D,M,A),IDU,IDS,C ) ) :- ato( ID,data(D,X,A),IDU,IDS,C )),(  ato( ID,data(D,X,A),IDU,IDS,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao(  ato( ID,data(D,M,A),IDU,IDS,C ) ).

evoluirConhecimento( ato( ID,data(D,M,A),IDU,IDS,C ) ) :-
    demo(  ato( ID,data(D,M,A),IDU,IDS,C ),desconhecido),
    solucoes( (excecao(   ato( ID,data(D,M,A),IDU,IDS,C ) ) :- ato( ID,data(D,M,X),IDU,IDS,C )),(  ato( ID,data(D,M,X),IDU,IDS,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao(  ato( ID,data(D,M,A),IDU,IDS,C ) ).

evoluirConhecimento( ato( ID,D,IDU,IDS,C ) ) :-
	demo( ato( ID,D,IDU,IDS,C ),desconhecido),
    solucoes( (excecao(  ato( ID,D,IDU,IDS,C ) ) :-  ato( ID,X,IDU,IDS,C)),( ato( ID,X,IDU,IDS,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao( ato( ID,D,IDU,IDS,C )  ).

evoluirConhecimento( ato( ID,D,IDU,IDS,C ) ) :-
	demo( ato( ID,D,IDU,IDS,C ),desconhecido),
    solucoes( (excecao(  ato( ID,D,IDU,IDS,C ) ) :-  ato( ID,D,X,IDS,C)),( ato( ID,D,X,IDS,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao( ato( ID,D,IDU,IDS,C )  ).

evoluirConhecimento( ato( ID,D,IDU,IDS,C ) ) :-
	demo( ato( ID,D,IDU,IDS,C ),desconhecido),
    solucoes( (excecao(  ato( ID,D,IDU,IDS,C ) ) :-  ato( ID,D,IDU,X,C)),( ato( ID,D,IDU,X,C ),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao( ato( ID,D,IDU,IDS,C )  ).

evoluirConhecimento( ato( ID,D,IDU,IDS,C ) ) :-
	demo( ato( ID,D,IDU,IDS,C ),desconhecido),
    solucoes( (excecao(  ato( ID,D,IDU,IDS,C ) ) :-  ato( ID,D,IDU,IDS,X)),( ato( ID,D,IDU,IDS,X),nao( nulo( X ) ) ),LEXC ),
    comprimento(LEXC,S), S > 0,
    removeAll( LEXC ),
    removeAto( ID ),
    evolucao( ato( ID,D,IDU,IDS,C )  ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% perfeito positivo

inserirPositivo( Termo ) :-
	demo(Termo,falso),
	nao(existeE(Termo)),
	naoExisteNT(-Termo),
   	naoExiste(Termo),
    evolucao( Termo ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% perfeito positivo

inserirNegativo( -Termo ) :-
	demo(Termo,falso),
	nao(existeE(Termo)),	
	naoExiste(Termo),
    naoExisteNT(-Termo),
    evolucao(-Termo).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo incerto

inserirIncerto( utente( ID,I,M ),nome ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,XPTO,I,M ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,XPTO,IU,MU )) ).
inserirIncerto( utente( ID,N,M ),idade ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,N,XPTO,M ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,XPTO,MU )) ).
inserirIncerto( utente( ID,N,I ),morada ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,N,I,XPTO ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,IU,XPTO )) ).

inserirIncerto( cuidado( ID,I ),descricao ) :-
    getIncXPTO( XPTO ),
    evolucao( cuidado( ID,XPTO,I ) ),
    evolucao( (excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,XPTO,IC )) ).
inserirIncerto( cuidado( ID,D ),instituicao ) :-
    getIncXPTO( XPTO ),
    evolucao( cuidado( ID,D,XPTO ) ),
    evolucao( (excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,DC,XPTO )) ).

inserirIncerto( instituicao( D ),cidade ) :-
    getIncXPTO( XPTO ),
    evolucao( instituicao( D,XPTO ) ),
    evolucao( (excecao( instituicao( DI,CI ) ) :- instituicao( DI,XPTO )) ).

inserirIncerto( ato( ID,data( M,A ),IDU,IDS,C ),dia ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,data(XPTO,M,A),IDU,IDS,C ) ),
    evolucao( (excecao( ato( IDA,data(DA,MA,AA),IDUA,IDSA,CA ) ) :- ato( IDA,data(XPTO,MA,AA),IDUA,IDSA,CA )) ).
inserirIncerto( ato( ID,data( D,A) ,IDU,IDS,C ),mes ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,data(D,XPTO,A),IDU,IDS,C ) ),
    evolucao( (excecao( ato( IDA,data(DA,MA,AA),IDUA,IDSA,CA ) ) :- ato( IDA,data(DA,XPTO,AA),IDUA,IDSA,CA )) ).
inserirIncerto( ato( ID,data( D,M ),IDU,IDS,C ),ano ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,data(D,M,XPTO),IDU,IDS,C ) ),
    evolucao( (excecao( ato( IDA,data(DA,MA,AA),IDUA,IDSA,CA ) ) :- ato( IDA,data(DA,MA,XPTO),IDUA,IDSA,CA )) ).
inserirIncerto( ato( ID,IDU,IDS,C ),data ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,XPTO,IDU,IDS,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,XPTO,IDUA,IDSA,CA )) ).
inserirIncerto( ato( ID,D,IDS,C ),utente ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,XPTO,IDS,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,XPTO,IDSA,CA )) ).
inserirIncerto( ato( ID,D,IDU,C ),cuidado ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,IDU,XPTO,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,IDUA,XPTO,CA )) ).
inserirIncerto( ato( ID,D,IDS,C ),custo ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,IDU,IDS,XPTO ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,IDUA,IDSA,XPTO )) ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo impreciso

inserirImpreciso( utente( ID,N,[I1 - I2],M ) ) :-
    evolucao( (excecao( utente( ID,N,I,M ) ) :- I >= I1, I =< I2) ).
inserirImpreciso( utente( ID,[],I,M ) ).
inserirImpreciso( utente( ID,N,[],M ) ).
inserirImpreciso( utente( ID,N,I,[] ) ).
inserirImpreciso( utente( ID,[N|NS],I,M ) ) :-
    evolucao( excecao( utente( ID,N,I,M ) ) ),
    inserirImpreciso( utente( ID,NS,I,M ) ).
inserirImpreciso( utente( ID,N,[I|IS],M ) ) :-
    evolucao( excecao( utente( ID,N,I,M ) ) ),
    inserirImpreciso( utente( ID,N,IS,M ) ).
inserirImpreciso( utente( ID,N,I,[M|MS] ) ) :-
    evolucao( excecao( utente( ID,N,I,M ) ) ),
    inserirImpreciso( utente( ID,N,I,MS ) ).

inserirImpreciso( cuidado( ID,[],I ) ).
inserirImpreciso( cuidado( ID,D,[] ) ).
inserirImpreciso( cuidado( ID,[D|DS],I ) ) :-
    evolucao( excecao( cuidado( ID,D,I ) ) ),
    inserirImpreciso( cuidado( ID,DS,I ) ).
inserirImpreciso( cuidado( ID,D,[I|IS] ) ) :-
    evolucao( excecao( cuidado( ID,D,I ) ) ),
    inserirImpreciso( cuidado( ID,D,IS ) ).

inserirImpreciso( instituicao( D,[] ) ).
inserirImpreciso( instituicao( D,[C|CS] ) ) :-
    evolucao( excecao( instituicao( D,C ) ) ),
    inserirImpreciso( instituicao( D,CS ) ).

inserirImpreciso( ato( ID,D,IDU,IDS,[C1 - C2] ) ) :-
    evolucao( (excecao( ato( ID,D,IDU,IDS,C ) ) :- C >= C1, C =< C2) ).
inserirImpreciso( ato( ID,data( [D1 - D2],M,A ),IDU,IDS,C ) ) :-
    evolucao( (excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) :- D >= D1, D =< D2) ).
inserirImpreciso( ato( ID,data( D,[M1 - M2],A ),IDU,IDS,C ) ) :-
    evolucao( (excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) :- M >= M1, M =< M2) ).
inserirImpreciso( ato( ID,data( D,M,[A1 - A2] ),IDU,IDS,C ) ) :-
    evolucao( (excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) :- A >= A1, A =< A2) ).
inserirImpreciso( ato( ID,[],IDU,IDS,C ) ).
inserirImpreciso( ato( ID,D,[],IDS,C ) ).
inserirImpreciso( ato( ID,D,IDU,[],C ) ).
inserirImpreciso( ato( ID,D,IDU,IDS,[] ) ).
inserirImpreciso( ato( ID,data( [],M,A ),IDU,IDS,C ) ).
inserirImpreciso( ato( ID,data( D,[],A ),IDU,IDS,C ) ).
inserirImpreciso( ato( ID,data( D,M,[] ),IDU,IDS,C ) ).
inserirImpreciso( ato( ID,[D|DS],IDU,IDS,C ) ) :-
    evolucao( excecao( ato( ID,D,IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,DS,IDU,IDS,C ) ).
inserirImpreciso( ato( ID,D,[IDU|IDUS],IDS,C ) ) :-
    evolucao( excecao( ato( ID,D,IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,D,IDUS,IDS,C ) ).
inserirImpreciso( ato( ID,D,IDU,[IDS|IDSS],C ) ) :-
    evolucao( excecao( ato( ID,D,IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,D,IDU,IDSS,C ) ).
inserirImpreciso( ato( ID,D,IDU,IDS,[C|CS] ) ) :-
    evolucao( excecao( ato( ID,D,IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,D,IDU,IDS,CS ) ).
inserirImpreciso( ato( ID,data( [D|DS],M,A ),IDU,IDS,C ) ) :-
    evolucao( excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,data( DS,M,A ) ),IDU,IDS,C ).
inserirImpreciso( ato( ID,data( D,[M|MS],A ),IDU,IDS,C ) ) :-
    evolucao( excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,data( D,MS,A ) ),IDU,IDS,C ).
inserirImpreciso( ato( ID,data( D,M,[A|AS] ),IDU,IDS,C ) ) :-
    evolucao( excecao( ato( ID,data( D,M,A ),IDU,IDS,C ) ) ),
    inserirImpreciso( ato( ID,data( D,M,AS ) ),IDU,IDS,C ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo interdito

inserirInterdito( utente( ID,I,M ),nome ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,XPTO,I,M ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,XPTO,IU,MU )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+utente( IDU,NU,IU,MU ) :: (solucoes( X,(utente( ID,X,_,_ ),nao( nulo( X ) )),S ),
                                           comprimento( S,L ),
                                           L == 0 )) ).
inserirInterdito( utente( ID,I,M ),idade ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,N,XPTO,M ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,XPTO,MU )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+utente( IDU,NU,IU,MU ) :: (solucoes( X,(utente( ID,_,X,_ ),nao( nulo( X ) )),S ),
                                           comprimento( S,L ),
                                           L == 0 )) ).
inserirInterdito( utente( ID,I,M ),morada ) :-
    getIncXPTO( XPTO ),
    evolucao( utente( ID,N,I,XPTO ) ),
    evolucao( (excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,IU,XPTO )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+utente( IDU,NU,IU,MU ) :: (solucoes( X,(utente( ID,_,_,X ),nao( nulo( X ) )),S ),
                                           comprimento( S,L ),
                                           L == 0 )) ).

inserirInterdito( cuidado( ID,I ),descricao ) :-
    getIncXPTO( XPTO ),
    evolucao( cuidado( ID,XPTO,I ) ),
    evolucao( (excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,XPTO,IC )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+cuidado( IDC,DC,IC ) :: (solucoes( X,(cuidado( ID,X,_ ),nao( nulo( X ) )),S ),
                                         comprimento( S,L ),
                                         L == 0 )) ).
inserirInterdito( cuidado( ID,D ),instituicao ) :-
    getIncXPTO( XPTO ),
    evolucao( cuidado( ID,D,XPTO ) ),
    evolucao( (excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,DC,XPTO )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+cuidado( IDC,DC,IC ) :: (solucoes( X,(cuidado( ID,_,X ),nao( nulo( X ) )),S ),
                                         comprimento( S,L ),
                                         L == 0 )) ).

inserirInterdito( instituicao( D ),cidade ) :-
    getIncXPTO( XPTO ),
    evolucao( instituicao( D,XPTO ) ),
    evolucao( (excecao( instituicao( DI,CI ) ) :- instituicao( DI,XPTO )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+instituicao( DI,CI ) :: (solucoes( X,(instituicao( DI,X ),nao( nulo( X ) )),S ),
                                         comprimento( S,L ),
                                         L == 0 )) ).

inserirInterdito( ato( ID,IDU,IDS,C ),data ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,XPTO,IDU,IDS,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,XPTO,IDUA,IDSA,CA )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+ato( IDA,DA,IDUA,IDSA,CA ) :: (solucoes( X,(ato( ID,X,_,_,_ ),nao( nulo( X ) )),S ),
                                               comprimento( S,L ),
                                               L == 0 )) ).
inserirInterdito( ato( ID,D,IDS,C ),utente ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,XPTO,IDS,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,XPTO,IDSA,CA )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+ato( IDA,DA,IDUA,IDSA,CA ) :: (solucoes( X,(ato( ID,_,X,_,_ ),nao( nulo( X ) )),S ),
                                               comprimento( S,L ),
                                               L == 0 )) ).
inserirInterdito( ato( ID,D,IDU,C ),cuidado ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,IDU,XPTO,C ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,IDUA,XPTO,CA )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+ato( IDA,DA,IDUA,IDSA,CA ) :: (solucoes( X,(ato( ID,_,_,X,_ ),nao( nulo( X ) )),S ),
                                               comprimento( S,L ),
                                               L == 0 )) ).
inserirInterdito( ato( ID,D,IDS,C ),custo ) :-
    getIncXPTO( XPTO ),
    evolucao( ato( ID,D,IDU,IDS,XPTO ) ),
    evolucao( (excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,IDUA,IDSA,XPTO )) ),
    evolucao( nulo( XPTO ) ),
    evolucao( (+ato( IDA,DA,IDUA,IDSA,CA ) :: (solucoes( X,(ato( ID,_,_,_,X ),nao( nulo( X ) )),S ),
                                               comprimento( S,L ),
                                               L == 0 )) ).



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
% Extensão do predicado que permite a evolução de uma lista de Termos

insertAll( [] ).
insertAll( [H|T] ) :-
    assert( H ),
    insertAll( T ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de uma lista de Termos

removeAll( [] ).
removeAll( [H|T] ) :-
    retract( H ),
    removeAll( T ).



%---------------------------------- - - - - - - - - - -  -  -  -  -  -
% Predicado «comprimento» que calcula o número de elementos
% existentes numa lista

comprimento( L,S ) :-
    length( L,S ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concat: Lista1, Lista2, R -> {V,F}

concat( [],L,L ).
concat( [X|Xs],L2,[X|L] ) :-
    concat( Xs,L2,L ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento, Lista -> {V,F}

pertence( X,[X|L]).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removeUtente: IDU -> {V,F}

removeUtente( ID ) :-
    solucoes( utente( IDU,NU,IU,MU ),utente( ID,NU,IU,MU ),LUT ),
    comprimento( LUT,L ),
    L > 0,
    removeAll( LUT ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removeCuidado: IDC -> {V,F}

removeCuidado( ID ) :-
    solucoes( cuidado( IDC,DC,IC),cuidado( ID,DC,IC),LC ),
    comprimento( LC,L ),
    L > 0,
    removeAll( LC).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removeInst: D,I -> {V,F}

removeInst( D) :-
    solucoes( instituicao(DI,II),instituicao( D,II),LI),
    comprimento( LI,L ),
    L > 0,
    removeAll( LI).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removeCuidado: IDA -> {V,F}

removeAto( ID ) :-
    solucoes( ato( IDA,DA,IC,IS,C),ato( ID,DA,IC,IS,C),LA ),
    comprimento( LA,L ),
    L > 0,
    removeAll( LA).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado naoExiste: ID -> {V,F}

naoExiste(utente(ID,N,I,M)) :-
	solucoes( ID, utente(ID,NU,IU,MU), L),
    comprimento( L,S ),
   	S == 0.
naoExiste(instituicao(D,I)) :-
	solucoes( D, utente(D,I), L),
    comprimento( L,S ),
   	S == 0.
naoExiste(cuidado(ID,D,I)) :-
	solucoes( ID, cuidado(ID,DC,IC), L),
    comprimento( L,S ),
   	S == 0.
naoExiste(ato(ID,D,IDC,IDS,C)):-
	solucoes( ID, ato(ID,DA,IDCA,IDSA,CA), L),
    comprimento( L,S ),
   	S == 0.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado naoExisteNT: ID -> {V,F}

naoExisteNT(-utente(ID,N,I,M)) :-
	solucoes( ID, -utente(ID,NU,IU,MU), L),
    comprimento( L,S ),
   	S < 2.
naoExisteNT(-instituicao(D,I)) :-
	solucoes( D, -instituicao(D,II), L),
    comprimento( L,S ),
   	S < 2.
naoExisteNT(-cuidado(ID,D,I)) :-
	solucoes( ID, -cuidado(ID,DC,IC), L),
    comprimento( L,S ),
   	S < 2.
naoExisteNT(-ato(ID,D,IDC,IDS,C)):-
	solucoes( ID, -ato(ID,DA,IDCA,IDSA,CA), L),
    comprimento( L,S ),
   	S < 2.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado existeE: ID -> {V,F}

existeE(utente(ID,N,I,M)) :-
	retract((excecao(utente(ID,N,X,M)):- X>=Y,X=<Z)),
	assert((excecao(utente(ID,N,X,M)):- X>=Y,X=<Z)).
existeE(utente(ID,N,I,M)) :-
	solucoes( (excecao( utente( ID,NU,IU,MU ))),(excecao( utente( ID,NU,IU,MU ))),LEXC ),
    comprimento(LEXC,S), S > 0.
existeE(cuidado(ID,D,I)) :-
	solucoes( (excecao(cuidado(ID,DC,IC))),(excecao(cuidado(ID,DC,IC))),LEXC ),
    comprimento(LEXC,S), S > 0.
existeE(instituicao(D,I)) :-
	solucoes( (excecao(instituicao(D,II))),(excecao(instituicao(D,II))),LEXC ),
    comprimento(LEXC,S), S > 0.
existeE(ato(ID,D,IDU,IDS,C)) :-
	retract((excecao(ato(ID,D,IDU,IDS,X)):- X>=Y,X=<Z)),
	assert((excecao(ato(ID,D,IDU,IDS,X)):- X>=Y,X=<Z)).
existeE(ato(ID,data( D,M,A ),IDU,IDS,C)) :-
	retract((excecao(ato(ID,data( X,M,A ),IDU,IDS,C)):- X>=Y,X=<Z)),
	assert((excecao(ato(ID,data( X,M,A ),IDU,IDS,C)):- X>=Y,X=<Z)).
existeE(ato(ID,data( D,M,A ),IDU,IDS,C)) :-
	retract((excecao(ato(ID,data( D,X,A ),IDU,IDS,C)):- X>=Y,X=<Z)),
	assert((excecao(ato(ID,data( D,X,A ),IDU,IDS,C)):- X>=Y,X=<Z)).
existeE(ato(ID,data( D,M,A ),IDU,IDS,C)) :-
	retract((excecao(ato(ID,data( D,M,X ),IDU,IDS,C)):- X>=Y,X=<Z)),
	assert((excecao(ato(ID,data( D,M,X ),IDU,IDS,C)):- X>=Y,X=<Z)).
existeE(ato(ID,D,IDU,IDS,C)) :-
	solucoes( (excecao( ato(ID,DA,IDA,IDSA,CA))),(excecao(ato(ID,DA,IDA,IDSA,CA))),LEXC ),
    comprimento(LEXC,S), S > 0.