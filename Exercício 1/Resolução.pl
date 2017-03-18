%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Resolução do exercício 1 (Trabalho prático)
t
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
u
:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic ato/4.
:- dynamic instituicao/1.
:- dynamic cuidado/4.




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dados da base de conhecimento
utente( 1,diogo,21,braga ).
utente( 2,rui,20,braga ).
utente( 3,esm,21,prado ).
utente( 4,miguel,22,viana ).
utente( 5,joao,26,guimaraes).
utente( 6,lisandra,25, fafe).
utente( 7,paulo,24,braganca).

instituicao( hpbraga ).
instituicao( hsjoao ).
instituicao( hviana ).
instituicao( hporto ).
instituicao( hfaro ).

cuidado( 1,analises,hpbraga,braga ).
cuidado( 2,tac,hsjoao,porto ).
cuidado( 3,nascimento,hpbraga,braga ).
cuidado( 4,febre,hviana,hviana ).
cuidado( 5,dar-sangue,hpbraga,braga ).
cuidado( 6,raioX,hporto,porto ).
cuidado( 7,consulta,hporto,porto ).
cuidado( 8,nascimento,hfaro,faro ).
cuidado( 9,ecografia,hfaro,faro ).
cuidado(10,quimioterapia,hsjoao,porto).

ato( data( 1,2,1996 ),3,3,10 ).
ato( data( 15,3,2017 ),1,2,15 ).
ato( data( 17,4,1997 ),4,4,5 ).
ato( data( 15,3,2007 ),1,5,0 ).
ato( data( 15,3,2007 ),2,5,0 ).
ato( data( 15,3,2007 ),3,2,0 ).
ato( data( 16,3,2017 ),5,6,12 ).
ato( data( 16,3,2007 ),6,9,20 ).
ato( data( 16,3,2007 ),3,1,40 ).




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
                          comprimento(S, L), 
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




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os utentes por critérios de seleção;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pesquisaUtentes: 
% Opcao, Parametro, Lista -> {V,F}

pesquisaUtentes( id,P,L ) :-
    findall( utente( P,X,Y,Z ),utente( P,X,Y,Z ),L ).
pesquisaUtentes( nome,P,L ) :-
    findall( utente(X,P,Y,Z ),utente( X,P,Y,Z ),L ).
pesquisaUtentes( idade,P,L ) :-
    findall( utente(X,Y,P,Z ),utente( X,Y,P,Z ),L ).
pesquisaUtentes( morada,P,L ) :-
    findall( utente(X,Y,Z,P ),utente( X,Y,Z,P ),L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar as instituições prestadoras de cuidados de saúde;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaInst: Lista -> {V,F}
% Lista é a lista de todas as instituições a que estão associados
% cuidados medicos

listaInst( S ) :-
    findall( N,cuidado( _,_,N,_ ),L),
    tiraRepetidos( L, S ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os cuidados prestados por instituição/cidade;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaCui: Opcao, Parametro, Lista -> {V,F}
% Lista é a lista de todos os cuidados na cidade/instituição

listaCui( cidade, C, L) :-
    findall( (Y,Z),cuidado( X,Y,Z,C ),L).
listaCui( inst, I, L) :-
    findall( (Y,Z),cuidado( X,Y,I,Z ),L).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os utentes de uma instituição/servico

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaUtentes: Opcao, Parametro, Lista -> {V,F}
% Lista é a lista de todos os utentes com atos medicos na
% instituição/cuidado
% Parametro é a descrição da instituição/cuidado

listaUtentes( inst,I,L ) :- 
    procuraCui( inst,I,Temp ),
    procuraAtos( Temp,Temp2 ),
    procuraUtentes( Temp2,L ).
listaUtentes( cuid,C,L ) :- 
    procuraCui( cuid,C,Temp ),
    procuraAtos( Temp,Temp2 ),
    procuraUtentes( Temp2,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraCui: Tipo, Parametro, Lista -> {V,F}
% Lista é a lista dos IdsServ da instituição/cuidado
% Parametro é a descrição da instituição/cuidado

procuraCui( inst,I,L ) :-
    findall( X,cuidado( X,_,I,_ ),Temp ),
    tiraRepetidos( Temp,L ).
procuraCui( cuid,D,L ) :-
    findall( X,cuidado( X,D,_,_ ),Temp ),
    tiraRepetidos( Temp,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraAtos: ListaIdsServ, Lista -> {V,F}
% Lista é a lista dos IdsUt com serviços da ListaIdsServ

procuraAtos( [X],L ) :-
    findall( U,ato( _,U,X,_ ),L ).
procuraAtos( [H|T],L ) :-
    findall( U, ato( _,U,H,_ ),Temp ),
    procuraAtos( T,Temp2 ),
    concat( Temp,Temp2,Temp3 ),
    tiraRepetidos( Temp3,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado procuraUtentes: ListaIdsUt, Lista -> {V,F}
% Lista é a lista com (IdUt, Nome) dos utentes em ListaIdsUt

procuraUtentes( [X],L ) :-
    findall( ( X,N ),utente( X,N,_,_ ),L ).
procuraUtentes( [H|T],L ) :-
    findall( ( H,N ),utente( H,N,_,_ ),Temp ),
    procuraUtentes( T,Temp2 ),
    concat( Temp,Temp2,Temp3 ),
    tiraRepetidos( Temp3,L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os atos médicos realizados, por utente/instituição/serviço;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaAtosMed: Opcao, Parametro, Lista -> {V,F}
% Lista é a lista dos atos medicos do utente/instituição/serviço

listaAtosMed( utente,IDU,L ) :-
    findall( ato( D,IDU,IDC,C ),ato( D,IDU,IDC,C ),Temp ),
    tiraRepetidos( Temp,L ).
listaAtosMed( cuid,IDC,L ) :-
    findall( ato( D,IDU,IDC,C ),ato( D,IDU,IDC,C ),Temp ),
    tiraRepetidos( Temp,L ).
listaAtosMed( inst,I,L ) :-
    procuraCui( inst,I,Temp ),
    listarAtos( Temp,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listarAtos: ListaIdsServ, Lista -> {V,F}
% Lista é a lista dos atos medicos dos serviços em ListaIdsServ

listarAtos( [ID],L ) :-
    findall( ato( X,Y,ID,Z ),ato( X,Y,ID,Z ),L ).
listarAtos( [ID|T],L ) :-
    findall( ato( X,Y,ID,Z ),ato( X,Y,ID,Z ),Temp ),
    listarAtos( T,Temp2 ),
    concat( Temp,Temp2,Temp3 ),
    tiraRepetidos( Temp3,L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Determinar todas as instituições/serviços a que um utente já recorreu;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado histUtente: Tipo, id Utente, Lista -> {V,F}
% Lista é a lista das instituições/serviços a que o utente recorreu

histUtente( inst,ID,L ) :-
    atosCuidados( ID,Temp ),
    cuidadosInst( Temp,L ).
histUtente( cuid,ID,L) :-
    findall( IDS,ato( _,ID,IDS,_ ),Temp ),
    tiraRepetidos( Temp,L ).                

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado atosCuidados: IdUt, Lista -> {V,F}
% Lista é a lista com os IdServ a que o utente se sujeitou

atosCuidados( ID,L ) :-
    findall( IDS,ato( _,ID,IDS,_ ),Temp ),
    tiraRepetidos( Temp,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidadosInst: ListaIdsServ, Lista -> {V,F}
% Lista é a lista das instituições dos IdsServ em ListaIdsServ

cuidadosInst( [IDC],L ) :-
    findall( I,cuidado( IDC,_,I,_ ),L ).

cuidadosInst( [IDC|T],L ) :-
    findall( I,cuidado( IDC,_,I,_ ),Temp ),
    cuidadosInst( T,Temp2 ),
    concat( Temp,Temp2,Temp3 ),
    tiraRepetidos( Temp3,L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular o custo total dos atos medicos por 
% utente/serviço/instituição/data;

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado custo: Tipo,Parametro,Retorno -> {V,F}
% Parametro : IDutente, IDcuidado, Instituicao, Data, Mes, Ano, Mes&Ano

custo(u,IDU,R) :- 
    findall( C,ato( _,IDU,_,C ),L ),
	somatorio( L,R ).
custo(c,IDC,R) :-
    findall( C,ato( _,_,IDC,C ),L ),
	somatorio( L,R ).
custo( inst,I,R ) :- 
    findall( X,cuidado( X,_,I,_ ),L ),
    custoInstituicao(L,R).
custo( d,Data,R ) :-
    findall( C,ato( Data,_,_,C ),L ),
	somatorio( L,R ).
custo( m,Mes,R ) :-
    findall( C,(ato( D,_,_,C ), mes( D, Mes ) ),L ),
    somatorio( L,R ).
custo( a,Ano,R ) :-
    findall( C,(ato( D,_,_,C ), ano( D, Ano ) ),L ),
    somatorio( L,R ).
custo( ma,Mes,Ano,R ) :-
    findall( C,(ato( D,_,_,C ), mes( D, Mes ) , ano( D,Ano ) ),L ),
    somatorio( L,R ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado custoInst: ListaIdsServ, Custo -> {V,F}

custoInstituicao( [IDC],R ) :-
	findall( C,ato( _,_,IDC,C ),L ),
	somatorio( L,R ).

custoInstituicao( [IDC|T],R ) :-
	findall( C,ato( _,_,IDC,C ),L ),
	somatorio( L,Temp ),
	custoInstituicao( T,Temp2 ),
 	R is Temp + Temp2.




%------------------------ Funcionalidades Extra -------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituicoesServico: Serviço, Lista -> {V,F}
% Lista é a lista das instituições que disponibilizam um serviço

instituicoesServico( S,L ) :-
    findall( I,cuidado(_,S,I,_),L ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado servicoMaisUsado:
% Ano, (Serviço, NOcorrencias) -> {V,F}

servicoMaisUsado( A,(Desc,N) ) :- 
    findall( ( ID,NA ),( ato( D,_,ID,_ ),ano( D,A ),quantos( A,ID,NA ) ),L ),
    tuploMaximo( L,(IDS,N) ),
    findall( Descricao, cuidado( IDS,Descricao,_,_ ),Temp ),
    ultimo( Temp,Desc ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado quantos: Ano, IdServ, Numero -> {V,F}
% Número é o número de atos registado no Ano, para o serviço IdServ

quantos( A,ID,NA ) :- findall( ID, ( ato( D,_,ID,_ ),ano( D,A ) ),L ),
                      comprimento( L,NA ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado datasVisitaInst: NomeUt, Inst, Lista -> {V,F}
% Lista é a lista das datas em que o utente visitou a instituição

datasVisitaInst( N,I,L ) :-
    findall( D,( utente( IDU,N,_,_ ),ato( D,IDU,IDS,_ ),cuidado( IDS,_,I,_ ) ),L ).



%------------------------ Predicados Auxiliares -------------------------

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
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao,
    !, fail.

nao( Questao ).




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
