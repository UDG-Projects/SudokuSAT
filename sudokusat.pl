%%%%%%%%%%%%
% sat(F,I,M)
% si F es satisfactible, M sera el model de F afegit a la interpretació I (a la primera crida I sera buida).
% Assumim invariant que no hi ha literals repetits a les clausules ni la clausula buida inicialment.
sat([],I,I):-     write('SAT!!, SUDOKU PISCINAS!!! '),nl,!.
sat(CNF,I,M):-
    % Ha de triar un literal d’una clausula unitaria, si no n’hi ha cap, llavors un literal pendent qualsevol.
    decideix(CNF,Lit),

    % Simplifica la CNF amb el Lit triat (compte pq pot fallar, es a dir si troba la clausula buida fallara i fara backtraking).
    simplif(Lit,CNF,CNFS),

    % crida recursiva amb la CNF i la interpretacio actualitzada
	  append(I,[Lit],ISEG),
    sat(CNFS,ISEG ,M).


%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% decideix(F, Lit)
% Donat una CNF,
% -> el segon parametre sera un literal de CNF
%  - si hi ha una clausula unitaria sera aquest literal, sino
%  - un qualsevol o el seu negat.
decideix(L,A):- append(_,[X|_], L), append([A],[],X),!. % detectar clàusula unitària
decideix([PL|_], A) :- append([A],_,PL).                % Retorna el primer element
decideix([PL|_], B) :- append([A],_,PL), B is 0-A.      % Retorna el negat del primer element


%%%%%%%%%%%%%%%%%%%%%  DONE MACARRONE!!
% simplif(Lit, CNF, CNFS)
% Donat un literal Lit i una CNF,
% -> el tercer parametre sera la CNF que ens han donat simplificada:
%  - sense les clausules que tenen lit
%  - treient -Lit de les clausules on hi es, si apareix la clausula buida fallara.
simplif(_, [], []).                                                                    % QUAN TINGUEM LA LLISTA BUIDA RETORNEM LA LLISTA BUIDA.
simplif(Lit, [PRIMERA|CUA], CNFS) :- member(Lit, PRIMERA),  							             % Comprovem si conté lit a primera
                                     simplif(Lit, CUA, CNFS), !. 						           % seguim amb la cua.
simplif(Lit, [PRIMERA|CUA], CNFS) :- X is 0-Lit, 										                   % neguem el literal i el fotem a x
                                     append(A, [X|XS], PRIMERA), append(A, XS, RES),   % Treure el literal negat a primera serà res
                                     simplif(Lit, CUA, CUACNFS), 						           % crida recursiva
                                     append([RES], CUACNFS, CNFS), !. 					       % Muntem la cadena final
simplif(Lit, [PRIMERA|CUA], CNFS) :- simplif(Lit, CUA, CUACNFS), 						           % Crida recursiva quan no conté Lit
                                     append([PRIMERA], CUACNFS, CNFS), !. 				     % Concatenar resultats

%%%%%%%%%%%%%% DONE MACARRONE!!
% negat(A,ANEG)
% Donat una llista de literals enters torna una llista amb els literals enters negats.
negat([],[]).
negat([A],[ANEG]):- ANEG is 0-A,!.
negat([A|CUA], [ANEG|CUANEG]):- negat([A],[ANEG]), negat(CUA,CUANEG).

%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% combina(L,COMBINAT)
% COMBINAT és la llista resultant de combinar el primer element de L amb la resta
% L -> és una llista d'elements a tractar
combina([],[]).
combina([A],[[A]]).
combina([A,B],[[A,B]]).
combina(L, COMBINAT):- append([A,B], CUA, L),
					   append([A],CUA,PIVOTA),
					   combina(PIVOTA,ITSEG), append([[A,B]],ITSEG,COMBINAT),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% montaParelles(L, PARELLES)
% PARELLES és el resultat de combinar tots els elements de L entre sí.
montaParelles([],[]).
montaParelles([A,B],[[A,B]]):-!.
montaParelles(L, PARELLES):- append([_],CUA,L),
							 combina(L,PIVOTA),
							 montaParelles(CUA,ITSEG),
							 append(PIVOTA,ITSEG,PARELLES),!.


%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% exactamentUn(L,CNF)
% Donat una llista de variables booleanes,
% -> el segon parametre sera la CNF que codifica que exactament una sigui certa.
exactamentUn([],[]).
exactamentUn([A],[[A]]).
exactamentUn(L,CNF):- negat(L, LNEG), montaParelles(LNEG,PARELLES), append([L],PARELLES, CNF).

%%%%%%%%%%%%%% DONE MACARRONE!!
% allDiff(L,F)
% Donat una llista (L) de Kdominis,
% -> CNF  codifica que no poden prendre el mateix valor.
% - Extraurem el primer literal de cada k-domini
% - muntem una llista amb tots els literals extrets
% - cridem a alldif amb totes les cues.
allDiff(L,CNF):-matTransposa(L,T), iAllDiff(T,CNF).


%%%%%%%%%%%%%%%%
% INMERSIÓ (AHUUUUUHA)
% iAllDiff(L,CNF)
% Donada una llista de caselles L
% -> CNF serà la clausla que valida que totes les caselles son diferents
iAllDiff([],[]).
iAllDiff(L,CNF):-append([PCOL],MAT,L), negat(PCOL, PCOLNEG), montaParelles(PCOLNEG, PCOLNEGCOMB),
        iAllDiff(MAT, CNFPARCIAL), append(PCOLNEGCOMB, CNFPARCIAL, CNF),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% taulerSudoku(N,C,Tauler,CNF)
% Donat el domini de les caselles (N), una inicialitzacio del Sudoku (de forma [c[1,2,5],...]),
% -> Tauler serà Llista de llistes de N-dominis del tauler,
% -> i CNF codifica els valors assignats amb clausules unitaries que forcen valor a les caselles inicialitzades
taulerSudoku(N,C,Tauler,CNF):-
    festauler(N,Tauler),
    inicialitzar(N,C,CNF).

%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% generaLLista(P, U, LL)
% Genera una llista de números de P fins a U i ho posa a LL
% -> P < U.
% -> P és l'inici
% -> U és el nombre final
% -> LL és la llista d'enters de P fins a U. [P..U]
generaLLista(P, P, [P]).
generaLLista(P, U, LL):- P < U,  K is P+1, generaLLista(K, U, R), append([P], R, LL),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% treuN(N, L, EXTRETS, CUA)
% Divideix L en EXTRETS i CUA, on EXTRETS conté N elements de L i CUA la resta.
% N -> nombre d'elements a extreure
% L -> Llista d'elements a tractar
% EXTRETS -> N elements de L en forma de LLISTA
% CUA -> L - els N primers elements de L en forma de LLISTA
treuN(0, L, [], L).
treuN(N, L, EXTRETS, CUA):- K is N-1, K >= 0, append([A], B, L), treuN(K, B, NOUEX, CUA), append([A], NOUEX, EXTRETS), !.

%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% separaPerN(N, L, R)
% Donada una llista L d'elements, R serà L dividida en K parts de N elements cada part.
% -> N és el nombre d'elements que han de tenir les subllistes
% -> L és la llista a tractar
% -> R és una llista de llistes de N elements cada subllista.
separaPerN(_, [], []).
separaPerN(N, L, R):- treuN(N, L, EXTRETS, CUA), separaPerN(N, CUA, NOUEXTRETS), append([EXTRETS], NOUEXTRETS, R), !.

%%%%%%%%%%%%%%%% DONE MACARRONE!!
% festauler(N,T)
% Donat el domini de les caselles (N),
% -> T sera una llista de N llistes de N llistes de N nombres
%    es a dir, una llista de files de N-dominis.
%    El primer N-domini tindra la forma [1,2,...N] i aixi successivament de fila en fila fins
%    a l'ultim N-domini corresponent a la casella (N,N) que sera [N*N*(N-1)+1,N*N*(N-1)+2,...N*N*N]
% festauler(0, []). %F is N*N, K is F*N
festauler(N, T):- K is N*N*N, generaLLista(1, K, LL), separaPerN(N, LL, LLN), separaPerN(N, LLN, T), !.

%%%%%%%%%%%%%%%%%%%%%%
% inicialitzar(N,LI,F)
% Donat el domini de les caselles (N), i la llista d'inicialitzacions (de forma [c[1,2,5],...]),
% -> el tercer parametre sera la CNF formada de clausules unitaries que forcen els valors corresponents
%    a les caselles corresponents (als N-dominis corresponents)
inicialitzar(_,[],[]):-!.
inicialitzar(N,LI,CNF):- append([c(F,C,D)],Resta,LI),
                         POS is N*N*(F-1) + (C-1)*N+D,
                         inicialitzar(N,Resta,W),
                         append([[POS]],W,CNF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% codificaSudoku(N,Tauler,C0,CNF)
% Donat el domini de les caselles (N) d'un Sudoku, el seu tauler i la CNF que l'inicialitza (C0),
% -> el quart parametre sera la CNF que codifica el Sudoku.
codificaSudoku(N,Tauler,C0,CNF):-  kdominis(Tauler,C1),
                                   allDiffFiles(Tauler,C2),
                                   allDiffColumnes(Tauler,C3),
                                   allDiffQuadrats(Tauler,N,C4),
                                   append(C1,C2,C12), append(C3,C4,C34),
                                   append(C0,C12,C012), append(C012,C34,CNF).



%%%%%%%%%%%%%%%
% kdominis(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica els exactamentUn per cada casella (K-domini)
kdominis([],[]).
kdominis([[]|T],F):-kdominis(T,F).
kdominis(T,F):- append([PF],CUA,T), append([PE],CUAFILA,PF), exactamentUn(PE,CNF1),
                append([CUAFILA],CUA,RESTA),kdominis(RESTA,CNFFINAL), append(CNF1,CNFFINAL,F),!.


%%%%%%%%%%%%%%%%%%%
% allDiffFiles(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada fila
allDiffFiles([],[]).
allDiffFiles(T,F):- append([PF],CUA,T),
                    allDiff(PF,CNF),
                    allDiffFiles(CUA,CNFCUA),
                    append(CNF,CNFCUA,F),!.

%%%%%%%%%%%%%%%%%%%%%%%%
% matTransposa(MAT,RES)
% Donat una matriu MAT.
% -> RES es la matriu transposada de MAT
matTransposa([],[]):-!.
matTransposa(MAT,RES):- append([PF],CUA,MAT),
                        transposarFila(PF,PFT),
                        matTransposa(CUA,REST),
                        matUnio(PFT,REST,RES),!.

%%%%%%%%%%%%%%%%%%%%%%
% matUnio(ESQ,DRE,MAT)
% Donades 2 matrius ESQ i PDRE
% -> MAT es la unió per files de les 2 matrius
% -> EX: matUnio([[1]],[[2],[3]],[[1,2],[3]])
matUnio([],T,T).
matUnio(T,[],T).
matUnio(ESQ,DRE,MAT):-append([PESQ],CUAESQ,ESQ), append([PDRE],CUADRE,DRE), append(PESQ,PDRE,PF),
                      matUnio(CUAESQ,CUADRE,MATRES),
                      append([PF],MATRES,MAT),!.

%%%%%%%%%%%%%%%%%%%%%%%%
% transposarFila(L,RES)
% Donada una llista (fila) L
% -> RES es el resultat de dividir la fila en columnes i retorna com una matriu
transposarFila([],[]).
transposarFila(L,RES):- append([P],CUA,L),
                        transposarFila(CUA,CUARES),
                        append([[P]],CUARES,RES).

%%%%%%%%%%%%%%%%%%%%%%
% allDiffColumnes(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada columna
allDiffColumnes(MAT,F):- matTransposa(MAT,T),
                         allDiffFiles(T,F),!.


%%%%%%%%%%%%%%%%%%%%%%%%
% allDiffQuadrats(T,N,F)
% Donat un Tauler, i la mida del K-domini (que es tambe el nombre de quadrats que hi ha),
% -> el tercer parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada quadrat
allDiffQuadrats([],_,[]).
allDiffQuadrats(T,N,F):- iAllDiffQuadrats(T,N,0,F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iAllDiffQuadrats(T,N,M,F)
% Inmersió per allDiffQuadrats.

iAllDiffQuadrats(_,N,N,[]).
iAllDiffQuadrats(T,N,M,F):- X is M+1, subset(T,N,M,S), allDiff(S,CNF), iAllDiffQuadrats(T,N,X,CNFX), append(CNF,CNFX,F),!.

%%%%%%%%%%%%%%%%%
% subset(T,N,Q,S)
% Serveix per extreure Un subquadrat amb el valor SUBQUADRAT.
% T és tauler
% N és el valor del sudoku.
% S Serà el subset referent al subquadrat de numero subquadrat.
% -----------------
% | 1 | 1 | 2 | 2 |
% | 1 | 1 | 2 | 2 |
% | 3 | 3 | 4 | 4 |
% | 3 | 3 | 4 | 4 |
% -----------------
% SUBQUADRAT pot prendre valor de 1 - N on en la matriu expressada anteriorment cada valor
% refereix al subquadrat marcat amb els nombres de 1 a 4.
subset(T,N,SUBQUADRAT,S):- P is truncate(sqrt(N)), X is (SUBQUADRAT mod P)*P, Y is (SUBQUADRAT div P)*P, extreu(T,X,Y,N,S).

%%%%%%%%%%%%%%%%%%%%%
% extreu(T,XI,YI,N,S)
% Extreu el subquadrat que estigui entre XI - XI + SQRT(N) i  YI - YI + SQRT(N) on X refereix a files i Y a columnes.
% T és Tauler
% XI és Fila inicial
% YI és columna inicial
% N és el valor del SUDOKU
% S serà el subquadrat de tamany SQRT(N) que està entre  XI - XI + SQRT(N) i  YI - YI + SQRT(N).
extreu(T,XI,YI,N,S):-ARR is truncate(sqrt(N)), XF is XI + ARR, YF is YI+ARR, iExtreu(T,XI,YI,XF,YF,0,0,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iExtreu(T,XI,YI,XF,YF,X,Y,S)
% Inmersió d'extreu. Retorna el subquadrat entre XI i XF, YI i YF.
% T és Tauler
% XI és Fila inici, XF és fila Final, X és Fila actual
% YI és columna inici, YF és columna Final, Y és columna actual
% S és el subquadrat format per tots els elements entre XI i XY, YI i YF.
iExtreu(_,_,_,_,YF,_,YF,[]). %ja ha trobat tot el quadrat
iExtreu(T,XI,YI,XF,YF,XF,Y,S):-   append([_],CUAF,T), YSEG is Y+1,
                                  iExtreu(CUAF,XI,YI,XF,YF,0,YSEG,S).   % me passat de columnes (X) puc saltar a la linia seguent
iExtreu(T,XI,YI,XF,YF,X,Y,S):-    append([PF],CUAF,T), append([PC], CUAC, PF),
                                  dinsRang(XI,XF,YI,YF,X,Y), append([CUAC],CUAF,MAT), XSEG is X+1,
                                  iExtreu(MAT,XI,YI,XF,YF,XSEG,Y,Q), append([PC],Q,S),!.
iExtreu(T,XI,YI,XF,YF,X,Y,S):-    append([PF],CUAF,T), append([_], CUAC, PF), append([CUAC],CUAF,MAT),  X<XF, XS is X+1,
                                  iExtreu(MAT,XI,YI,XF,YF,XS,Y,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dinsRang(XI,XF,YI,YF,X,Y)
% Comprova que X i Y estan entre XI i XF, YI i YF
% XI és Fila inici, XF és fila Final, X és Fila actual
% YI és columna inici, YF és columna Final, Y és columna actual
dinsRang(XI,XF,YI,YF,X,Y):-  X>=XI, X<XF, Y>=YI, Y<YF.


%%%%%%%%%%%%%%%%%
% resol(N,Inputs)
% Donada la N del sudoku (NxN), i una llista d'inputs,
% -> es mostra la solucio per pantalla si en te o es diu que no en te.
resol(N,Inputs):- nl, write('NEM A RESOLDRE EL SUDOKU : '),nl,
                  write('....................................'),nl,
                  mostraSudoku(N,Inputs), taulerSudoku(N, Inputs, T, C0), codificaSudoku(N,T,C0,CNF),
                  sat(CNF,[],M), mostra(M,N),!.

%%%%%%%%%%%%%%%%%%%%%%
% mostraSudoku(N, IN)
% Mostra un sudoku expressat en format llistat de c(F,C,V) per pantalla.
% N és el tamany del sudoku
% IN és el sudoku representat en llistat de c(F,C,V).
mostraSudoku(N, IN) :- pintaSeparador(N), iMostraSudoku(1,1,N,IN).

%%%%%%%%%%%%%%%%%%%%%%%%%
% iMostraSudoku(F,C,N,IN)
% inmersió per mostraSudoku.
% F és la fila actual, C és la columna actual
% N és el tamany del SUDOKU
% IN és el llistat de caselles que té amb valor el sudoku
% IN ha d'estar ordenada per files i columnes.
iMostraSudoku(F,_,N,[]):- F>N, nl, !.
iMostraSudoku(F,C,N,IN):- C>N, write('|'), nl, FS is F+1,
                          pintaSeparador(N), iMostraSudoku(FS, 1, N, IN).
iMostraSudoku(F,C,N,IN):- pintaCasella(F,C,IN,INSEG), CSEG is C+1,
                          iMostraSudoku(F, CSEG, N, INSEG), !.
iMostraSudoku(F,C,N,IN):- write('|'), write(' '), CSEG is C+1,
                          iMostraSudoku(F, CSEG, N, IN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pintaCasella(F,C,IN, FCUA)
% Pinta el valor de una casella amb el separador al davant si coincideixen F = IN[F] i C = IN[C].
% F és el valor de la fila, C és el valor de la Columna
% IN son les caselles possibles a pintar per la casella [F,C]
% FCUA seran les caselles restants si es compleix que es pot pintar.
pintaCasella(F,C,IN, FCUA) :- append([c(FC,CC,VC)], FCUA, IN), F=FC, C=CC,
                              write('|'), write(VC).

%%%%%%%%%%%%%%%%%%%
% pintaSeparador(N)
% Pinta un separador generat amb '-' amb tamany N+2+N-1 o N*2+1 caràcters
pintaSeparador(N):- NF is (N+2)+(N-1), iPintaSeparador(1, NF), nl.

%%%%%%%%%%%%%%%%%%%%%%%%
% iPintaSeparador(N, NF)
% Crida inmersiva del pintaSeparador.
iPintaSeparador(N, NF):- N>NF, !.
iPintaSeparador(N, NF):- write('-'), NSEG is N+1, iPintaSeparador(NSEG, NF).


% Donat un Model per a un encoding d'un Sudoku de NxN, ens mostra els valors finals.
% (s'assumeix que s'han codificat els K-dominis, etc com es requereix a la practica).
mostra(M,N):- write('....................................'),nl, pintaSeparador(N), mostraM(M,1,N).

mostraM(_,F,N):-N is F-1,!.
mostraM(M,F,N):-mostraFila(M,F,1,N), write('|'), nl, pintaSeparador(N), F1 is F+1, mostraM(M,F1,N).

mostraFila(_,_,C,N):- C>N,!.
mostraFila(M,F,C,N):-   LB is (F-1)*N*N + (C-1)*N, UB is LB+N,
                        member(X,M),X>LB, X=<UB, V is X-LB, write('|'), write(V),
                        Cp is C+1, mostraFila(M,F,Cp,N).



% exemple sudoku 9x9
% [c(1,4,2), c(1,5,6),c(1,7,7),c(1,9,1),c(2,1,6), c(2,2,8),c(2,5,7),c(2,8,9),c(3,1,1), c(3,2,9), c(3,6,4), c(3,7,5),c(4,1,8), c(4,2,2), c(4,4,1), c(4,8,4),c(5,3,4), c(5,4,6), c(5,6,2), c(5,7 ,9), c(6,2,5), c(6,6,3), c(6,8,2), c(6,9,8),c(7,3,9), c(7,4,3), c(7,8,7), c(7,9 ,4), c(8,2,4), c(8,5,5), c(8,8,3), c(8,9,6), c(9,1,7), c(9,3,3), c(9,5,1), c(9,6,8)]
