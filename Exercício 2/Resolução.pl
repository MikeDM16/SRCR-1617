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

counter_idu(100).
counter_ids(100).
counter_ida(100).
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


getXPTO( XPTO ) :-
    counter_xpto( XPTO2 ),
    name( XPTO,[120,112,116,111,XPTO2] ).

getIDU( IDU ) :-
    counter_idu( IDU ).

getIDS( IDS ) :-
    counter_ids( IDS ).

getIDA( IDA ) :-
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

ato( 25,data(1,4,2017),25,10,xpto007 ).
excecao( ato( ID,D,IDU,IdS,P ) ) :-
    ato( ID,D,IDU,IdS,xpto007 ).
nulo( xpto007 ).

+ato( ID,D,IDU,IdS,P ) :: ( solucoes( P,( ato( 25,_,_,_,P ),nao( nulo( P ) ) ),S ),
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
% perfeito

evolucao( Termo ) :-
    solucoes( INV,+Termo::INV,LINV ),
    insercao( Termo ),
    testa( LINV ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo incerto

evolucaoIncerto( utente( N,I,M ),[P|Ps] ) :-
    getIncIDU( IDU ),
    insereUtente( utente( IDU,N,I,M ),[P|Ps],[],[] ).
evolucaoIncerto( cuidado( D,I ),[P|Ps] ) :-
    getIncIDS( IDS ),
    insereCuidado( cuidado( IDS,D,I ),[P|Ps],[],[] ).
evolucaoIncerto( instituicao( D,C ),[P|Ps] ) :-
    insereInstituicao( instituicao( D,C ),[P|Ps],[],[] ).
evolucaoIncerto( ato( D,IDU,IDS,C ),[P|Ps] ) :-
    getIncIDA( IDA ),
    insereAto( ato( IDA,D,IDU,IDS,C ),[P|Ps],[],[] ).

insereUtente( utente( ID,N,I,M ),[],LPRE,LEXC ) :-
    concat( LPRE,LEXC,L ),
    inserir( L ).
insereUtente( utente( ID,N,I,M ),[nome|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [utente( ID,XPTO,I,M)],LPRE,LPRE1 ),
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,XPTO,I,M ))],LEXC,LEXC1 ),
    insereUtente( utente( ID,N,I,M ),T,LPRE1,LEXC1 ).
insereUtente( utente( ID,N,I,M ),[idade|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [utente( ID,N,XPTO,M)],LPRE,LPRE1 ),
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,N,XPTO,M ))],LEXC,LEXC1 ),
    insereUtente( utente( ID,N,I,M ),T,LPRE1,LEXC1 ).
insereUtente( utente( ID,N,I,M ),[morada|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [utente( ID,N,I,XPTO)],LPRE,LPRE1 ),
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,N,I,XPTO ))],LEXC,LEXC1 ),
    insereUtente( utente( ID,N,I,M ),T,LPRE1,LEXC1 ).

insereCuidado( cuidado( ID,D,I ),[],LPRE,LEXC ) :-
    concat( LPRE,LEXC,L ),
    inserir( L ).
insereCuidado( cuidado( ID,D,I ),[descricao|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [cuidado( ID,XPTO,I)],LPRE,LPRE1 ),
    concat( [(excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,XPTO,I ))],LEXC,LEXC1 ),
    insereCuidado( cuidado( ID,D,I ),T,LPRE1,LEXC1 ).
insereCuidado( cuidado( ID,D,I ),[instituicao|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [cuidado( ID,D,XPTO)],LPRE,LPRE1 ),
    concat( [(excecao( cuidado( IDC,DC,IC ) ) :- cuidado( IDC,D,XPTO ))],LEXC,LEXC1 ),
    insereCuidado( cuidado( ID,D,I ),T,LPRE1,LEXC1 ).

insereInstituicao( instituicao( D,C ),[],LPRE,LEXC ) :-
    concat( LPRE,LEXC,L ),
    inserir( L ).
insereInstituicao( instituicao( D,C ),[cidade|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [instituicao( D,XPTO )],LPRE,LPRE1 ),
    concat( [(excecao( instituicao( DI,CI ) ) :- instituicao( DI,XPTO ))],LEXC,LEXC1 ),
    insereInstituicao( instituicao( D,I ),T,LPRE1,LEXC1 ).

insereAto( ato( ID,D,IDU,IDS,C ),[],LPRE,LEXC ) :-
    concat( LPRE,LEXC,L ),
    inserir( L ).
insereAto( ato( ID,D,IDU,IDS,C ),[data|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [ato( ID,XPTO,IDU,IDS,C )],LPRE,LPRE1 ),
    concat( [(excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,XPTO,IDU,IDS,C ))],LEXC,LEXC1 ),
    insereAto( ato( ID,D,IDU,IDS,C ),T,LPRE1,LEXC1 ).
insereAto( ato( ID,D,IDU,IDS,C ),[utente|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [ato( ID,D,XPTO,IDS,C )],LPRE,LPRE1 ),
    concat( [(excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,DA,XPTO,IDS,C ))],LEXC,LEXC1 ),
    insereAto( ato( ID,D,IDU,IDS,C ),T,LPRE1,LEXC1 ).
insereAto( ato( ID,D,IDU,IDS,C ),[cuidado|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [ato( ID,D,IDU,XPTO,C )],LPRE,LPRE1 ),
    concat( [(excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,D,IDU,XPTO,C ))],LEXC,LEXC1 ),
    insereAto( ato( ID,D,IDU,IDS,C ),T,LPRE1,LEXC1 ).
insereAto( ato( ID,D,IDU,IDS,C ),[custo|T],LPRE,LEXC ) :-
    getIncXPTO( XPTO ),
    concat( [ato( ID,D,IDU,IDS,XPTO )],LPRE,LPRE1 ),
    concat( [(excecao( ato( IDA,DA,IDUA,IDSA,CA ) ) :- ato( IDA,D,IDU,IDS,XPTO ))],LEXC,LEXC1 ),
    insereAto( ato( ID,D,IDU,IDS,C ),T,LPRE1,LEXC1 ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo impreciso

evolucaoImpreciso( utente( N,I,M ),E,[P|Ps] ) :-
    getIncIDU( IDU ),
    insereUtente2( utente( IDU,N,I,M ),E,[P|Ps],[]).
evolucaoImpreciso( cuidado( D,I ),[P|Ps] ) :-
    getIncIDS( IDS ),
    insereCuidado2( cuidado( IDS,D,I ),E,[P|Ps],[]).
evolucaoImpreciso( instituicao( D,C ),E,[P|Ps] ) :-
    insereInstituicao2( instituicao( D,C ),E,[P|Ps],[]).
evolucaoImpreciso( ato( D,IDU,IDS,C ),E,[P|Ps] ) :-
    getIncIDA( IDA ),
    insereAto2( ato( IDA,D,IDU,IDS,C ),E,[P|Ps],[]).

evolucaoImprecisoInt( utente( N,I,M ),idade,Linf,Lsup ) :-
    getIncIDU( ID ),
    evolucao( ( excecao( utente( ID,N,IS,M ) ) :- IS >= Linf, IS =< Lsup ) ).
evolucaoImprecisoInt( ato( ID,D,IDU,IDS,C ),custo,Linf,Lsup ) :-
    getIncIDA( IDA ),
    evolucao( ( excecao( ato( IDA, D,IDU,IDS,CS ) ) :- CS >= Linf, CS =< Lsup ) ).

insereUtente2( utente( ID,N,I,M ),P,[],LEXC) :-
    inserir( LEXC).
insereUtente2( utente( ID,N,I,M ),nome,[H|T],LEXC ) :-
    concat( [(excecao(utente( ID,H,I,M)) )],LEXC,LEXC1 ),
    insereUtente2( utente( ID,N,I,M ),nome,T,LEXC1 ).
insereUtente2( utente( ID,N,I,M ),idade,[H|T],LEXC ) :-
    concat( [(excecao(utente( ID,N,H,M)))],LEXC,LEXC1 ),
    insereUtente2( utente( ID,N,I,M ),idade,T,LEXC1 ).
insereUtente2( utente( ID,N,I,M ),morada,[H|T],LEXC ) :-
    concat( [(excecao(utente( ID,N,I,H)))],LEXC,LEXC1 ),
    insereUtente2( utente( ID,N,I,M ),morada,T,LEXC1 ).

insereCuidado2( cuidado( ID,D,I ),P,[],LEXC) :-
    inserir( LEXC).
insereCuidado2( cuidado( ID,D,I ),descricao,[H|T],LEXC ) :-
    concat( [(excecao(cuidado( ID,H,I)) )],LEXC,LEXC1 ),
    insereCuidado2( cuidado( ID,D,I),descricao,T,LEXC1 ).
insereCuidado2( cuidado( ID,D,I ),instituicao,[H|T],LEXC ) :-
    concat( [(excecao(cuidado( ID,D,H)))],LEXC,LEXC1 ),
    insereCuidado2( cuidado( ID,D,I),instituicao,T,LEXC1 ).

insereInstituicao2( instituicao( D,C ),P,[],LEXC) :-
    inserir( LEXC).
insereInstituicao2( instituicao(D,C),descricao,[H|T],LEXC ) :-
    concat( [(excecao(instituicao(H,C)) )],LEXC,LEXC1 ),
    insereInstituicao2( instituicao(D,C),descricao,T,LEXC1 ).
insereInstituicao2( instituicao( D,C ),cidade,[H|T],LEXC ) :-
    concat( [(excecao(instituicao(D,H)))],LEXC,LEXC1 ),
    insereInstituicao2( instituicao(D,C),instituicao,T,LEXC1 ).

insereAto2( ato( ID,D,IDU,IDS,C ),P,[],LEXC) :-
    inserir( LEXC).
insereAto2( ato( ID,D,IDU,IDS,C ),data,[H|T],LEXC ) :-
    concat( [(excecao(ato( ID,H,IDU,IDS,C )))],LEXC,LEXC1 ),
    insereAto2(ato( ID,D,IDU,IDS,C ),data,T,LEXC1 ).
insereAto2( ato( ID,D,IDU,IDS,C ),utente,[H|T],LEXC ) :-
    concat( [(excecao(ato( ID,D,H,IDS,C )))],LEXC,LEXC1 ),
    insereAto2(ato( ID,D,IDU,IDS,C ),utente,T,LEXC1 ).
insereAto2( ato( ID,D,IDU,IDS,C ),cuidado,[H|T],LEXC ) :-
    concat( [(excecao(ato( ID,D,IDU,H,C )))],LEXC,LEXC1 ),
    insereAto2(ato( ID,D,IDU,IDS,C ),cuidado,T,LEXC1 ).
insereAto2( ato( ID,D,IDU,IDS,C ),custo,[H|T],LEXC ) :-
    concat( [(excecao(ato( ID,D,IDU,IDS,H )))],LEXC,LEXC1 ),
    insereAto2(ato( ID,D,IDU,IDS,C ),custo,T,LEXC1 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolução de conhecimento
% imperfeito do tipo interdito

evolucaoInterdito( utente( N,I,M ),[P|Ps] ) :-
    getIncIDU( IDU ),
    insereUtente3( utente( IDU,N,I,M ),[P|Ps],[],[],[] ).
evolucaoInterdito( instituicao( D,C ),[P|Ps] ) :-
    insereInstituicao3( instituicao( D,C ),[P|Ps],[],[],[] ).
evolucaoInterdito( cuidado( D,Ins), [P|Ps] ) :-
    getIncIDA( IDA ),
    insereCuidado3( cuidado( D,Ins ),[P|Ps],[],[],[] ).


insereUtente3( utente( N,I,M ),[],LEXC,LNUL,LINV ) :-
    getIncIDU( IDU ),
    evolucao( utente( IDU,N,I,M ) ),
    insertAll( LEXC ),
    insertAll( LNUL ),
    insertAll( LINV ).
insereUtente3( utente( N,I,M ),[idade|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    getIDU( IDAA ),
    IDA is IDAA + 1,
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,XPTO,MU ))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteUtente( IDA,XPTO,idade,LINV,LINV1 ),
    insereUtente3( utente( N,XPTO,M ),T,LEXC1,LNUL1,LINV1 ).
insereUtente3( utente( N,I,M ),[nome|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    getIDU( IDAA ),
    IDA is IDAA + 1,
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,XPTO,IU,MU ))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteUtente( IDA,XPTO,nome,LINV,LINV1 ),
    insereUtente3( utente( XPTO,I,M ),T,LEXC1,LNUL1,LINV1 ).
insereUtente3( utente( N,I,M ),[morada|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    getIDU( IDAA ),
    IDA is IDAA + 1,
    concat( [(excecao( utente( IDU,NU,IU,MU ) ) :- utente( IDU,NU,IU,XPTO ))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteUtente( IDA,XPTO,morada,LINV,LINV1 ),
    insereUtente3( utente( N,I,XPTO ),T,LEXC1,LNUL1,LINV1 ).

criaInvarianteUtente( IDU,XPTO,idade,L,LINV ) :-
    concat( [( +utente( ID,N,I,M ) :: ( solucoes( X,( utente( IDU,_,XPTO,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
criaInvarianteUtente( IDU,XPTO,nome,L,LINV ) :-
    concat( [( +utente( ID,N,I,M ) :: ( solucoes( X,( utente( IDU,XPTO,_,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
criaInvarianteUtente( IDU,XPTO,morada,L,LINV ) :-
    concat( [( +utente( ID,N,I,M ) :: ( solucoes( X,( utente( IDU,_,_,XPTO ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).


insereInstituicao3( instituicao( D,C ),[],LEXC,LNUL,LINV ) :-
    evolucao( instituicao( D,C ) ),
    insertAll( LEXC ),
    insertAll( LNUL ),
    insertAll( LINV ).
insereInstituicao3( instituicao( D,C ),[cidade|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    concat( [(excecao( instituicao( DI,CI ) ) :- instituicao( DI,XPTO ))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteInstituicao( D,XPTO,cidade,LINV,LINV1),
    insereInstituicao2( instituicao( D,XPTO ),T,LEXC1,LNUL1,LINV1 ).

criaInvarianteUtente( IDU,XPTO,idade,L,LINV ) :-
    concat( [( +utente( ID,N,I,M ) :: ( solucoes( X,( utente( IDU,_,XPTO,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
criaInvarianteUtente( IDU,XPTO,nome,L,LINV ) :-
    concat( [( +utente( ID,N,I,M ) :: ( solucoes( X,( utente( IDU,XPTO,_,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
criaInvarianteInstituicao( D,XPTO,cidade,L,LINV ) :-
    concat( [( +instituicao( D,C ) :: ( solucoes( X,( instituicao( D,XPTO ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).


insereCuidado3( cuidado( D,Ins ),[],LEXC,LNUL,LINV ) :-
    getIncIDS( IDS ),
    evolucao( cuidado( IDS, D,Ins ) ),
    insertAll( LEXC ),
    insertAll( LNUL ),
    insertAll( LINV ).
insereCuidado3( cuidado( D,Ins ),[descricao|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    getIDS( IDAA ),
    IDA is IDAA + 1,
    concat( [(excecao( cuidado(IDS, DS, InsS) ) :- cuidado(IDS,XPTO,InsS))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteCuidado( IDA,XPTO,descricao,LINV,LINV1 ),
    insereCuidado2( cuidado( XPTO,Ins ),T,LEXC1,LNUL1,LINV1 ).
insereCuidado3( cuidado( D,Ins ),[instituicao|T],LEXC,LNUL,LINV ) :-
    getIncXPTO( XPTO ),
    getIDS( IDAA ),
    IDA is IDAA + 1,
    concat( [(excecao( cuidado(IDS, DS, InsS ) ) :- cuidado(IDS,DS, XPTO ))],LEXC,LEXC1 ),
    concat( [(nulo( XPTO ))],LNUL,LNUL1 ),
    criaInvarianteCuidado( IDA,XPTO,instituicao,LINV,LINV1 ),
    insereCuidado2( cuidado( D,XPTO ),T,LEXC1,LNUL1,LINV1 ).

criaInvarianteCuidado( IDU,XPTO,descricao,L,LINV ) :-
    concat( [( +cuidado( ID,D,I,C ) :: ( solucoes( X,( cuidado( IDU,XPTO,_,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
criaInvarianteCuidado( IDU,XPTO,instituicao,L,LINV ) :-
    concat( [( +cuidado( ID,D,I,C ) :: ( solucoes( X,( cuidado( IDU,_,XPTO,_ ),nao( nulo( XPTO ) ) ),S ),
                                        comprimento( S,L ),
                                        L == 0 ) )],L,LINV).
                                    

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

inserir( L1 ) :-
    insertAll( L1,[] ).


insertAll( [],_ ).
insertAll( [H|T],L ) :-
    assert( H ),
    concat( [H],L,L1 ),
    insertAll( T,L1 ).
insertAll( L1,L2 ) :-
    removeAll( L2 ), !, fail.


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