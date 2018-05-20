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
    sat(... , ... ,M).


%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% decideix(F, Lit)
% Donat una CNF,
% -> el segon parametre sera un literal de CNF
%  - si hi ha una clausula unitaria sera aquest literal, sino
%  - un qualsevol o el seu negat.
decideix([[A]], A).
decideix([PL|CL], A) :- append([A], [], PL), !.
decideix([PL|CL], A) :- CL \= [], decideix(CL, A), !.
decideix([PL|CL], A) :- append([A],XS,PL).

%%%%%%%%%%%%%%%%%%%%%  DONE MACARRONE!!
% simplif(Lit, CNF, CNFS)
% Donat un literal Lit i una CNF,
% -> el tercer parametre sera la CNF que ens han donat simplificada:
%  - sense les clausules que tenen lit
%  - treient -Lit de les clausules on hi es, si apareix la clausula buida fallara.
simplif(_, [], []). % QUAN TINGUEM LA LLISTA BUIDA RETORNEM LA LLISTA BUIDA.
simplif(Lit, [PRIMERA|CUA], CNFS) :- member(Lit, PRIMERA),  % Comprovem si conté lit a primera
                                     simplif(Lit, CUA, CNFS), !. % seguim amb la cua.
simplif(Lit, [PRIMERA|CUA], CNFS) :- X is 0-Lit, % neguem el literal i el fotem a x
                                     append(A, [X|XS], PRIMERA), append(A, [XS], RES),  % Treure el literal negat a primera serà res
                                     simplif(Lit, CUA, CUACNFS), % crida recursiva
                                     append(RES, CUACNFS, CNFS), !. % Muntem la cadena final
simplif(_, [PRIMERA|[]], [PRIMERA]). % Hem de filtrar la llista buida del final
simplif(Lit, [PRIMERA|CUA], CNFS) :- simplif(Lit, CUA, CUACNFS), % Crida recursiva quan no conté Lit
                                     append([PRIMERA], CUACNFS, CNFS), !. % Concatenar resultats

%%%%%%%%%%%%%%
% negat(A,ANEG)
% Donat un literal enter torna el seu negat
negat([A], [ANEG]) :- ANEG is 0-A.
negat([A,B], [C,D]) :-   negat([A],[C]), negat([B],[D]) .

combinaINega([],[]).
combinaINega([A,B],[NEGATS]):- negat([A,B],NEGATS),!.
combinaINega(L,NEGATS):- append([A,B], CUA, L),
                         negat([A,B], ITNEGAT), append([A],CUA, PIVOTA),
                         combinaINega(PIVOTA,ITSEG), append([ITNEGAT],ITSEG,NEGATS),!.

montaParelles([],[]).
montaParelles([A,B], [NEGATS]):- negat([A,B],NEGATS),!.
montaParelles(L, PARELLES):- append([A],CUA,L), combinaINega(L,PIVOTA), montaParelles(CUA,ITSEG), append(PIVOTA,ITSEG,PARELLES),!.

%montaParelles([], []).
%montaParelles([A,B], PARELLA) :- negat([A,B], PARELLA).
%montaParelles(L, PARELLES) :- append([A], CUA, L), append([B], BCUA,CUA), append([A],BCUA, EXCLOUB),
%                              negat([A,B], PARELLA),
%                              montaParelles(EXCLOUB, PARELLESA),
%                              montaParelles(CUA, PARELLESB),
%                              append(PARELLA, PARELLESA, P), append(P, PARELLESB, PARELLES).

%%%%%%%%%%%%%%%%%%%%%
% exactamentUn(L,CNF)
% Donat una llista de variables booleanes,
% -> el segon parametre sera la CNF que codifica que exactament una sigui certa.
% exactamentUn([A], CNF) :-  append([[A]], [[B]], CNF), negat(A,B). %  P ^ !P...
% exactamentUn([A,B], CNF) :- append([[A,B]], [[C, D]], CNF), C is 0-A, D is 0-B.
%exactamentUn([], []).
%exactamentUn([A,B], CNF) :- negat([A,B], NEGAT), append([A,B], NEGAT, CNF).
%exactamentUn(L, CNF) :- append([A], CUA, L), write(A), nl, exactamentUn(CUA, CNF).
%exactamentUn(L, CNF) :- append(PRIMERS, ULTIMS, L),
%exactamentUn(L, CNF) :- append([A], CUA, L), append([B], BCUA,CUA), append([A],BCUA, RESTA),
%                        exactamentUn(RESTA , CNF), exactamentUn(CUA, CNF)
%                        , negat(A,C), negat(B,D),
%                        append(C,D,NEGATS).
%exactamentUn(L, CNF) :- append(P, [A|CUA], L), append(P, A, R), append(R, [B|BCUA], CUA),
%                        append(C, D, NEGATS), negat(A,C), negat(B,D),
%                        append([L], [NEGATS], CNF).




%%%%%%%%%%%%%%
% allDiff(L,F)
% Donat una llista (L) de Kdominis,
% -> CNF  codifica que no poden prendre el mateix valor.

% allDiff([[]|_],[]).
% allDiff(L,CNF):- ...


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
