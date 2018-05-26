%%%%%%%%%%%%
% sat(F,I,M)
% si F es satisfactible, M sera el model de F afegit a la interpretació I (a la primera crida I sera buida).
% Assumim invariant que no hi ha literals repetits a les clausules ni la clausula buida inicialment.

sat([],I,I):-     write('SAT!!'),nl,!.
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
decideix([PL|_], A) :- append([A],_,PL). % Retorna el primer element
decideix([PL|_], B) :- append([A],_,PL), B is 0-A. % Retorna el negat del primer element


%%%%%%%%%%%%%%%%%%%%%  DONE MACARRONE!!
% simplif(Lit, CNF, CNFS)
% Donat un literal Lit i una CNF,
% -> el tercer parametre sera la CNF que ens han donat simplificada:
%  - sense les clausules que tenen lit
%  - treient -Lit de les clausules on hi es, si apareix la clausula buida fallara.
simplif(_, [], []). % QUAN TINGUEM LA LLISTA BUIDA RETORNEM LA LLISTA BUIDA.
simplif(Lit, [PRIMERA|CUA], CNFS) :- member(Lit, PRIMERA),  							% Comprovem si conté lit a primera
                                     simplif(Lit, CUA, CNFS), !. 						% seguim amb la cua.
simplif(Lit, [PRIMERA|CUA], CNFS) :- X is 0-Lit, 										% neguem el literal i el fotem a x
                                     append(A, [X|XS], PRIMERA), append(A, XS, RES),  	% Treure el literal negat a primera serà res
                                     simplif(Lit, CUA, CUACNFS), 						% crida recursiva
                                     append([RES], CUACNFS, CNFS), !. 					% Muntem la cadena final
simplif(Lit, [PRIMERA|CUA], CNFS) :- simplif(Lit, CUA, CUACNFS), 						% Crida recursiva quan no conté Lit
                                     append([PRIMERA], CUACNFS, CNFS), !. 				% Concatenar resultats

%%%%%%%%%%%%%%
% negat(A,ANEG)
% Donat una llista de literals enters torna una llista amb els literals enters negats.
negat([],[]).
negat([A],[ANEG]):- ANEG is 0-A,!.
negat([A|CUA], [ANEG|CUANEG]):- negat([A],[ANEG]), negat(CUA,CUANEG).

%%%%%%%%%%%%%%%%%%%%%
% combina(L,COMBINAT)
% ...
combina([],[]).
combina([A],[[A]]).
combina([A,B],[[A,B]]).
combina(L, COMBINAT):- append([A,B], CUA, L),
					   append([A],CUA,PIVOTA),
					   combina(PIVOTA,ITSEG), append([[A,B]],ITSEG,COMBINAT),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% montaParelles(L, PARELLES)
% ...
montaParelles([],[]).
montaParelles([A,B],[[A,B]]):-!.
montaParelles(L, PARELLES):- append([_],CUA,L),
							 combina(L,PIVOTA),
							 montaParelles(CUA,ITSEG),
							 append(PIVOTA,ITSEG,PARELLES),!.


%%%%%%%%%%%%%%%%%%%%%
% exactamentUn(L,CNF)
% Donat una llista de variables booleanes,
% -> el segon parametre sera la CNF que codifica que exactament una sigui certa.
exactamentUn([],[]).
exactamentUn([A],[[A]]).
exactamentUn(L,CNF):- negat(L, LNEG), montaParelles(LNEG,PARELLES), append([L],PARELLES, CNF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matExtreuPrimers(MAT, LLP, LLCUES)
% Donada una matriu, retorna per separat la primera columna i la matriu restant.
% -> MAT És la matriu que tractem
% -> LLP És la llista amb els elements de la primera columna de mat
% -> LLCUES És la matriu resultant a l'extracció.
matExtreuPrimers([[]], [], []).
matExtreuPrimers(MAT, LLP, LLCUES):- append([PRL], MATCUA, MAT), append([PR], CUA, PRL),
                                     matExtreuPrimers(MATCUA, LLPRT, LLCUEST),
                                     append(PR, LLPRT, LLP), append(CUA, LLCUEST, LLCUES).

%%%%%%%%%%%%%%
% allDiff(L,F)
% Donat una llista (L) de Kdominis,
% -> CNF  codifica que no poden prendre el mateix valor.
%allDiff([[]|_],[]).
%allDiff(L,CNF):-

% - Extraurem el primer literal de cada k-domini
% - muntem una llista amb tots els literals extrets
% - cridem a alldif amb totes les cues.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% taulerSudoku(N,C,Tauler,CNF)
% Donat el domini de les caselles (N), una inicialitzacio del Sudoku (de forma [c[1,2,5],...]),
% -> Tauler serà Llista de llistes de N-dominis del tauler,
% -> i CNF codifica els valors assignats amb clausules unitaries que forcen valor a les caselles inicialitzades

taulerSudoku(N,C,Tauler,CNF):-
    festauler(N,Tauler),
    inicialitzar(N,C,CNF).


%%%%%%%%%%%%%%%%
% festauler(N,T)
% Donat el domini de les caselles (N),
% -> T sera una llista de N llistes de N llistes de N nombres
%    es a dir, una llista de files de N-dominis.
%    El primer N-domini tindra la forma [1,2,...N] i aixi successivament de fila en fila fins
%    a l'ultim N-domini corresponent a la casella (N,N) que sera [N*N*(N-1)+1,N*N*(N-1)+2,...N*N*N]


%%%%%%%%%%%%%%%%%%%%%%
% inicialitzar(N,LI,F)
% Donat el domini de les caselles (N), i la llista d'inicialitzacions (de forma [c[1,2,5],...]),
% -> el tercer parametre sera la CNF formada de clausules unitaries que forcen els valors corresponents
%    a les caselles corresponents (als N-dominis corresponents)

% inicialitzar(_,[],[]):-!.
% inicialitzar(N,...,...):- ...


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



%%%%%%%%%%%%%%%%%%%
% allDiffFiles(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada fila




%%%%%%%%%%%%%%%%%%%%%%
% allDiffColumnes(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada columna



%%%%%%%%%%%%%%%%%%%%%%%%
% allDiffQuadrats(T,N,F)
% Donat un Tauler, i la mida del K-domini (que es tambe el nombre de quadrats que hi ha),
% -> el tercer parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada quadrat






%%%%%%%%%%%%%%%%%
% resol(N,Inputs)
% Donada la N del sudoku (NxN), i una llista d'inputs,
% -> es mostra la solucio per pantalla si en te o es diu que no en te.




% Donat un Model per a un endoding d'un Sudoku de NxN, ens mostra els valors finals.
% (s'assumeix que s'han codificat els K-dominis, etc com es requereix a la practica).

mostra(M,N):- write('....................................'),nl, mostraM(M,1,N).

mostraM(_,F,N):-N is F-1,!.
mostraM(M,F,N):-mostraFila(M,F,1,N), nl, F1 is F+1, mostraM(M,F1,N).

mostraFila(M,F,C,N):- C>N,!.
mostraFila(M,F,C,N):-   LB is (F-1)*N*N + (C-1)*N, UB is LB+N,
                        member(X,M),X>LB, X=<UB, V is X-LB, write(V),
                        Cp is C+1, mostraFila(M,F,Cp,N).



% exemple sudoku 9x9
% [c(1,4,2), c(1,5,6),c(1,7,7),c(1,9,1),c(2,1,6), c(2,2,8),c(2,5,7),c(2,8,9),c(3,1,1), c(3,2,9), c(3,6,4), c(3,7,5),c(4,1,8), c(4,2,2), c(4,4,1), c(4,8,4),c(5,3,4), c(5,4,6), c(5,6,2), c(5,7 ,9), c(6,2,5), c(6,6,3), c(6,8,2), c(6,9,8),c(7,3,9), c(7,4,3), c(7,8,7), c(7,9 ,4), c(8,2,4), c(8,5,5), c(8,8,3), c(8,9,6), c(9,1,7), c(9,3,3), c(9,5,1), c(9,6,8)]
