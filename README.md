---
title: "Sudoku Sat - Pràctica Prolog"
author: [Francesc Xavier Bullich Parra i Marc Sànchez Pifarré, GEINF (UDG-EPS)]
date: 2018-06-13
subject: "Udg - Eps"
tags: [Sudoku, Prolog]
subtitle: "Tutor de la pràctica : Mateu Villaret"
titlepage: true
titlepage-color: 3C9F53
titlepage-text-color: FFFFFF
titlepage-rule-height: 4
...

# Informe sobre la pràctica Prolog.

En aquest informe s'hi allotja tot el codi degudament documentat sobre la pràctica prolog anomenada com sudokusat. Aquest informe es construeix sobre un exemple en concret de un sudoku de n=4. Amb aquest exemple s'anirà escenificant què ocorre a cada un dels predicats que considerem més important.

També consta d'un apartat d'exemples d'execució amb sudokus N=9.

# Índex :


# Propietats Sudoku

Com a propietats importants d'un sudoku tenim :

- Només podem fer sudokus de tamany N on N és un nombre que té una arrel entera. Per tant es poden fer sudokus de N=4, N=9, N=16...

# Predicats de la pràctica.

En aquest apartat es mostren tots els predicats que no son el mostra, el mostraM i el mostraLinia. S'expliquen i es posa un exemple d'execució de cada un d'ells. Així es pot comprovar el seu funcionament i evaluar si el tall està ben posat.

## Resol

Aquest predicat seria com el main del programa, és qui delega feina als predicats de manera ordenada i qui estructura les execucions per que el sat rebi els paràmetres que requereix i per que els sudokus es pintin de manera correcte.

### Predicat

S'hi ha afegit en tot el codi una série de writes per modificar la visualització del sudoku i fer-la més visual.

```
%%%%%%%%%%%%%%%%%
% resol(N,Inputs)
% Donada la N del sudoku (NxN), i una llista d'inputs,
% -> es mostra la solucio per pantalla si en te o es diu que no en te.
resol(N,Inputs):- nl, write('NEM A RESOLDRE EL SUDOKU : '),nl,
                  write('....................................'),nl,
                  mostraSudoku(N,Inputs), taulerSudoku(N, Inputs, T, C0), codificaSudoku(N,T,C0,CNF),
                  sat(CNF,[],M), mostra(M,N),!.
```

### Execució :

Veiem l'execució següent que és la que utilitzarem a tot l'informe per explicar de manera adequada el funcionament de cada un dels predicats.

```
| ?- resol(4,[c(1,1,3),c(2,2,1),c(4,4,3)]).

NEM A RESOLDRE EL SUDOKU :
....................................
---------
|3| | | |
---------
| |1| | |
---------
| | | | |
---------
| | | |3|
---------

SAT!!, SUDOKU PISCINAS!!!
....................................
---------
|3|2|1|4|
---------
|4|1|3|2|
---------
|2|3|4|1|
---------
|1|4|2|3|
---------
```

### Crides internes

- [Tauler Sudoku](tauler-sudoku)
- [Codifica Sudoku](codifica-sudoku)
- [Sat](sat)
- [Mostra](mostra)

## Tauler Sudoku

És un predicat que s'encarrega de generar un tauler a partir d'un nombre i una sèrie de inputs. Aquest tauler està representat en forma de llista de llistes de K-domins. On cada K-Domini es representa com a 4 variables que poden prendre el valor cert o fals. Llavors per representar que un valor és cert deixarem la variable com a enter positiva i per representar que és falça com un enter negatiu.

### Predicat :

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% taulerSudoku(N,C,Tauler,CNF)
% Donat el domini de les caselles (N), una inicialitzacio del Sudoku (de forma [c[1,2,5],...]),
% -> Tauler serà Llista de llistes de N-dominis del tauler,
% -> i CNF codifica els valors assignats amb clausules unitaries que forcen valor a les caselles inicialitzades
taulerSudoku(N,C,Tauler,CNF):-
  % Genera totes les possibles variables per cada domini dins de cada casella i cada llista.
  festauler(N,Tauler),
  % Agafa les caselles passades com a inputs C i genera la CNF que codifica aquestes caselles com a fixes.
  inicialitzar(N,C,CNF).
```

### Execució :

```
| ?- taulerSudoku(4, [c(1,1,3),c(2,2,1),c(4,4,3)], T, CNF).

CNF = [[3],[21],[63]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

En el resultat de l'execució podem veure que T té una llista de files on cada fila té una llista de caselles i on finalment per cada casella hi ha codificat la possibilitat de que prengui el valor 1, 2, 3 o 4 en forma de variables. Per la casella Fila 1 columna 1 el fet que hi hagi un 1 es representa amb un 1. A la casella Fila 1 columna 2 el fet que hi hagi un 1 es representa amb la variable 5.

D'aquesta manera CNF serà el llistat de clàusules que codifiquen que hem forçat un 3 a la fila 1, columna 1, un 1 a la fila 2 columna 2 i un 3 a la fila 4 columna 4.

### Crides internes

Es fa la crida al festauler i al inicialitzar.

- [Fes Tauler](fes-tauler)
- [Inicialitzar](inicialitzar)

## Fes Tauler

La idea és simple, es genera una llista de K on K és N^3 nombres. Un cop generada la llista la dividim en grups de N, dos vegades. És a dir sobre una llista de K varibales generem una llista LLN de K/N Llistes de N variables cada llista. I sobre la llista de K/N llistes dividim altre cop per N llistes de llistes quedant un llista de N llistes que té N llistes de N dominis on tot plegat son K dominis.

### Predicat

```
%%%%%%%%%%%%%%%% DONE MACARRONE!!
% festauler(N,T)
% Donat el domini de les caselles (N),
% -> T sera una llista de N llistes de N llistes de N nombres
%    es a dir, una llista de files de N-dominis.
%    El primer N-domini tindra la forma [1,2,...N] i aixi successivament de fila en fila fins
%    a l'ultim N-domini corresponent a la casella (N,N) que sera [N*N*(N-1)+1,N*N*(N-1)+2,...N*N*N]
% festauler(0, []). %F is N*N, K is F*N
festauler(N, T):- K is N*N*N, generaLLista(1, K, LL), separaPerN(N, LL, LLN), separaPerN(N, LLN, T), !.
```

### Execució

```
| ?- festauler(4,T).

T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

### Crides Internes

- [Genera Llista](genera-llista)
- [Separa Per N](separa-per-n)

## Genera llista

Generar una llista de K variables on K és N^3.

```
%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% generaLLista(P, U, LL)
% Genera una llista de números de P fins a U i ho posa a LL
% -> P < U.
% -> P és l'inici
% -> U és el nombre final
% -> LL és la llista d'enters de P fins a U. [P..U]
generaLLista(P, P, [P]).
generaLLista(P, U, LL):- P < U,  K is P+1, generaLLista(K, U, R), append([P], R, LL),!.
```

### Execució

```
| ?- generaLLista(1, 64, LL).

LL = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64]
```

## Separa Per N

Donats un N i una llista de K elements, divideix la llista de K elements en X subllistes de N elements.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% separaPerN(N, L, R)
% Donada una llista L d'elements, R serà L dividida en K parts de N elements cada part.
% -> N és el nombre d'elements que han de tenir les subllistes
% -> L és la llista a tractar
% -> R és una llista de llistes de N elements cada subllista.
separaPerN(_, [], []).
separaPerN(N, L, R):- treuN(N, L, EXTRETS, CUA), separaPerN(N, CUA, NOUEXTRETS), append([EXTRETS], NOUEXTRETS, R), !.
```


### Execució

```
| ?- separaPerN(4, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64], RESULTAT).

RESULTAT = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16],[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32],[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48],[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]
```

Un cop feta aquesta crida està tot dividit en caselles, s'ha de tornar a fer la mateixa crida per tenir dividida la llista dos vegades i així obtenir les files a partir de les caselles.

```
| ?- separaPerN(4, [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16],[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32],[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48],[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]], RESULTAT).

RESULTAT = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

### Crides Internes

- [Treu N](treu-n)


## Treu N

Divideix una llista en EXTRETS i CUA on EXTRETS és una llista de N elements de L i CUA és la resta expressada com a llista.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% treuN(N, L, EXTRETS, CUA)
% Divideix L en EXTRETS i CUA, on EXTRETS conté N elements de L i CUA la resta.
% N -> nombre d'elements a extreure
% L -> Llista d'elements a tractar
% EXTRETS -> N elements de L en forma de LLISTA
% CUA -> L - els N primers elements de L en forma de LLISTA
treuN(0, L, [], L).
treuN(N, L, EXTRETS, CUA):- K is N-1, K >= 0, append([A], B, L), treuN(K, B, NOUEX, CUA), append([A], NOUEX, EXTRETS), !.
```

Al final tenim una matriu cúbica representada en llistes aniuades on cada casella té una llista de varibles que representen la possibilitat que hi hagi un valor en concret en aquella casella.

### Execució

Fem un exemple concret i petit que no forma part de la resolució del sudoku de l'exemple.

```
| ?- treuN(4, [1,2,3,4,5,6,7,8], EXTRETS, CUA).

CUA = [5,6,7,8]
EXTRETS = [1,2,3,4]
```

## Inicialitzar

Aquest predicat tradueix els inputs representats en llista de c(F,C,V) en una CNF amb clàusules untàries on cada clàusula representa el fet que es fixi un valor en concret en el tauler generat.

### Predicat

```
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
```

### Execució

```
| ?- inicialitzar(4, [c(1,1,3),c(2,2,1),c(4,4,3)], CNF).
CNF = [[3],[21],[63]]
```

## Codifica sudoku

El que es proposa amb aquest predicat és generar les cnf que codifiquen que només hi ha un nombre possible per cada casella (kdominis), les cnf que codifiquen que tots els nombres de les caselles son diferents a cada una de les files, les cnf que codifiquen que tots els nombres de les caselles son diferents a cada una de les columnes i les cnf que codifiquen que tots els nombres de les caselles son diferents per cada un dels subquadrats del sudoku.

Els appends següents empalmen les diferents cnf per produïr la CNF del paràmetre per tot el sudoku.

### Predicat

```
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
```

### Execució

```
| ?- codificaSudoku(4, [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]],  [[3],[21],[63]], CNF).

CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
```

### Crides internes

- [K dominis](k-dominis)
- [All Diff Files](all-diff-files)
- [All Diff Columnes](all-diff-columnes)
- [All Diff Quadrats](all-diff-quadrats)

## K dominis

Reb el tauler i codifica el "exactament un" per a cada casella monta la codificació conforme només accepta un valor tal com es mostra en l'exemple d'execució següent.

### Predicat

```
%%%%%%%%%%%%%%%
% kdominis(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica els exactamentUn per cada casella (K-domini)
kdominis([],[]).
kdominis([[]|T],F):-kdominis(T,F).
kdominis(T,F):- append([PF],CUA,T), append([PE],CUAFILA,PF), exactamentUn(PE,CNF1),
                append([CUAFILA],CUA,RESTA),kdominis(RESTA,CNFFINAL), append(CNF1,CNFFINAL,F),!.
```

### Execució

Podem observar que la resposta CNF és part de la resposta CNF del predicat [codifica sudoku](codifica-sudoku).

```
| ?- kdominis([[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]], CNF).

CNF = [[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64]]

```

### Crides internes

- [Exactament Un](exactament-un)

## Exactament Un

Per cada una de les caselles és cridat aquest predicat que colabora en la generació de la CNF per codificar el sudoku sencer. Valida que una casella només prengui un dels seus valors possibles.

Una CNF "exactament un" que codifica un únic nombre per la primera casella d'un sudoku de N=4 pren la forma que es veu en l'exemple d'execució d'aquest mateix apartat. Aquest exemple d'execució es realitza sobre el cas explicat al llarg de l'informe peró només per una de les caselles del tauler.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%
% exactamentUn(L,CNF)
% Donat una llista de variables booleanes,
% -> el segon parametre sera la CNF que codifica que exactament una sigui certa.
exactamentUn([],[]).
exactamentUn([A],[[A]]).
exactamentUn(L,CNF):- negat(L, LNEG), montaParelles(LNEG,PARELLES), append([L],PARELLES, CNF).
```

### Execució

Veiem l'execució només per la primera casella d'un sudoku n=4.

```
| ?- exactamentUn([1,2,3,4],CNF).

CNF = [[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4]]
```

### Crides internes

- [Negat](negat)
- [Monta Parelles](monta-parelles)

## Negat

Les CNF's estan formades de llistes de llistes de k dominis on els k dominis son nombres enters. Cada nombre enter simbolitza un nombre donat una fila i una columna. Per tant codifica una variable, el fet de que la variable sigui certa o falça es representa amb un enter positiu o negatiu. La feina d'aquest predicat és la de capgirar el valor de les variables i passar-les de certes a falces o vicevesa. I per fer-ho s'utiltiza el 0 per restar-li el valor enter negatiu o positiu en funció de l'entrada.

### Predicat

```
%%%%%%%%%%%%%%
% negat(A,ANEG)
% Donat una llista de literals enters torna una llista amb els literals enters negats.
negat([],[]).
negat([A],[ANEG]):- ANEG is 0-A,!.
negat([A|CUA], [ANEG|CUANEG]):- negat([A],[ANEG]), negat(CUA,CUANEG).
```

### Execució

```
| ?- negat([1,-2,-3,4],NEGAT).

NEGAT = [-1,2,3,-4]
```

## Monta Parelles

La idea és que donada una llista amb n variables es cridi al predicat combina que genera una llista amb la combinació d'una de les variables amb la resta, fent així una llista de parelles de variables. Veure l'especificació del [Combina](combina).

La idea és que a mesura que es vagin combinant les variables no es tornint a combinar repetits i per tant s'exclou un cop combinada la variable i es torna a cridar el montaParelles amb la resta de variables que no poden haver-se combinat.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% montaParelles(L, PARELLES)
% PARELLES és el resultat de combinar tots els elements de L entre sí.
montaParelles([],[]).
montaParelles([A,B],[[A,B]]):-!.
montaParelles(L, PARELLES):- append([_],CUA,L),
              % [_] ja no ens importa per que ja s'ha combinat amb l'element extret.
							combina(L,PIVOTA), % [[1,2],[1,3],[1,4]]
							montaParelles(CUA,ITSEG),% Montarà amb [2,3,4]
							append(PIVOTA,ITSEG,PARELLES),!.
```

### Execució

```
| ?- montaParelles([1,2,3,4], PARELLES).

PARELLES = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
```

### Crides internes

- [Combina](combina)

## Combina

Donades una llista de variables, agafarà la primera i la combinarà amb la resta (només la primera). Ja es tornarà a cridar més endavant sense el primer element que ja s'ha combinat.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%
% combina(L,COMBINAT)
% COMBINAT és la llista resultant de combinar el primer element de L amb la resta
% L -> és una llista d'elements a tractar
combina([],[]).
combina([A],[[A]]).
combina([A,B],[[A,B]]).
combina(L, COMBINAT):- append([A,B], CUA, L),
					   append([A],CUA,PIVOTA),
					   combina(PIVOTA,ITSEG), append([[A,B]],ITSEG,COMBINAT),!.
```

### Execució

```
| ?- combina([1,2,3,4],COMBINAT).

COMBINAT = [[1,2],[1,3],[1,4]]
```


## All Diff Files

Controla que donades totes les files de la matriu, no hi hagi cap element repetit en cap de les files, complint així el primer dels tres propòsits d'un sudoku (No hi ha elements repetits a les files, no hi ha elements repetits a les columnes, no hi ha elements repetits als quadrats). CNF la llista de clàusules resultat que verifica que no hi ha cap repetit a les files.

### Predicat

```
%%%%%%%%%%%%%%%%%%%
% allDiffFiles(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada fila
allDiffFiles([],[]).
allDiffFiles(T,F):- append([PF],CUA,T),
                    allDiff(PF,CNF),
                    allDiffFiles(CUA,CNFCUA),
                    append(CNF,CNFCUA,F),!.
```

### Execució

```
| ?- allDiffFiles([[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]],CNF).

CNF = [[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64]]
```

### Crides internes

- [All Diff](all-diff)


## All Diff

All diff reb una llista de llistes de variables que representen un subconjunt de caselles. Aquest subconjunt de caselles pot ser un a fila, una columna o bé un quadrat. El que codifica el alldiff és que totes les caselles que entren siguin diferents. Com que el format d'entrada és una llista i nosaltres volem combinar les diferents opcions que hi ha dins del llistat de varibles el que fem és transposar aquest llistat per que el seu format passi d'aquí :

```
[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
```

A aquí :

```
[[1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16]]
```

Realitzant aquesta transposició ja podem accedir directament a vincular els diferents elements cridant al montaparelles per cada una de les possibilitats forçant que per exemple només hi hagi un 1, o només hi hagi un 2 o només hi hagi un 3 ... fin a N.  

### Predicat

```
%%%%%%%%%%%%%%
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
```

### Execució

Codifiquem la primera fila del sudoku N=4.

```
| ?- allDiff([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]], CNF).

CNF = [[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16]]
```

### Crides internes

- [Mat Transposa](mat-transposa)
- [Negat](negat)
- [Monta Parelles](monta-parelles)

## Mat Transposa

Donada qualsevol matriu, fa la seva transposada i la posa a RES. Per fer-ho es realitza fila per fila, fent que la fila es divideix en una fila per cada element i unint el resultat de cridar recursivament a la transposada de la cua per després unir-la per files.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%
% matTransposa(MAT,RES)
% Donat una matriu MAT.
% -> RES es la matriu transposada de MAT
matTransposa([],[]):-!.
matTransposa(MAT,RES):- append([PF],CUA,MAT),
                        transposarFila(PF,PFT),
                        matTransposa(CUA,REST),
                        matUnio(PFT,REST,RES),!.
```

### Execució

Fem un exemple de transposar la primera fila de una matriu de N=4.

```
| ?- matTransposa([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],TRANSPOSADA).

TRANSPOSADA = [[1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16]]
```

Un altre exemple seria transposar tot el tauler a nivell de Files i columnes, que tracta els dominins de cada casella no com una llista sinó com un element.

```
| ?- festauler(4,T), matTransposa(T, TRANSPOSADA).

T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
TRANSPOSADA = [[[1,2,3,4],[17,18,19,20],[33,34,35,36],[49,50,51,52]],[[5,6,7,8],[21,22,23,24],[37,38,39,40],[53,54,55,56]],[[9,10,11,12],[25,26,27,28],[41,42,43,44],[57,58,59,60]],[[13,14,15,16],[29,30,31,32],[45,46,47,48],[61,62,63,64]]]
```

### Crides internes

- [Transposar Fila](transposar-fila)
- [Mat Unió](mat-uni-)


## Transposar Fila

Donada una fila es retorna una llista de files on cada element de la llista L és una sola llista dins de RES.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%
% transposarFila(L,RES)
% Donada una llista (fila) L
% -> RES es el resultat de dividir la fila en columnes i retorna com una matriu
transposarFila([],[]).
transposarFila(L,RES):- append([P],CUA,L),
                        transposarFila(CUA,CUARES),
                        append([[P]],CUARES,RES), !.
```

### Execució

```
| ?- transposarFila([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]], FILA_TRANSPOSADA).

FILA_TRANSPOSADA = [[[1,2,3,4]],[[5,6,7,8]],[[9,10,11,12]],[[13,14,15,16]]]
```


## Mat Unió

Donades dues matrius ESQ i DRE, el matunió genera la unió de les dues matriu a partir de unir-les fila a fila. És a dir la matriu [[A],[C]] U [[B],[D]] = [[A,B],[C,D]].

Aquesta unió és genèrica per qualsevol matriu ESQ i qualsevol Matriu DRE.

### Predicat

```
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
```

### Execució

Els següents exemplifiquen el comportament del mat unió. (És la unió de dues matrius de tota la vida unides per files).

```
| ?- matUnio([[1],[2]],[[3]], RES).
RES = [[1,3],[2]]

| ?- matUnio([[1],[2]],[[[3]]], RES).
RES = [[1,[3]],[2]]

| ?- matUnio([[1]],[[3],[4]], RES).
RES = [[1,3],[4]]

| ?- matUnio([[1],[3]],[[],[4]], RES).
RES = [[1],[3,4]]
```


## All Diff Columnes

Simplement transposar la matriu que tenim, i cridar al all diff files. És un clar exemple d'abstracció.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%
% allDiffColumnes(T,F)
% Donat un Tauler,
% -> el segon parametre es la CNF que codifica que no hi hagi repetits (allDiff) als K-dominis de cada columna
allDiffColumnes(MAT,F):- matTransposa(MAT,T),
                         allDiffFiles(T,F),!.

```

### Execució

Veiem que ha transposat la matriu i que ha combinat els diferents parells de variables per que no hi hagi cap element repetit a les columnes.

```
| ?- festauler(4,T), allDiffColumnes(T, CNF).

CNF = [[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

### Crides internes

- [Mat Transposa](mat-transposa)
- [All diff Files](all-diff-files)

## All Diff Quadrats

Aquest predicta extreu un llistat de caselles per a cada possible quadrat del sudoku. Plantegem un sistema de quadrats basat en una fòrmula matemàtica utiltizant les arrels de N, on N és el nombre del sudoku.

Aquests quadrats segueixen la lògica expressada en el següent esquema :

```
|---|---|---|---|
| 0 | 0 | 1 | 1 |
| 0 | 0 | 1 | 1 |
| 2 | 2 | 3 | 3 |
| 2 | 2 | 3 | 3 |
|---|---|---|---|
```

D'aquesta manera el primer quadrat estarà format per les caselles c(1,1), c(1,2), c(2,1), c(2,2).

El procediment que es segueix és el d'agafar fila per fila i verificar que les caselles estan dins del subquadrat que es demana. Per aquest motiu s'utilitza inmersió. Per poder generar una llista de valors enters de 0 fins a N-1 que permeti demanar per cada valor el subquadrat X del sudoku on 0 <= X < N.

Utilitzem el subset per generar el llistat de caselles que s'han de combinar per generar la CNF corresponent a cada quadrat.

### Predicat

```
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
iAllDiffQuadrats(T,N,M,F):- X is M+1,
                            subset(T,N,M,S),
                            allDiff(S,CNF),
                            iAllDiffQuadrats(T,N,X,CNFX),
                            append(CNF,CNFX,F),!.
```

### Execució

```
| ?- N=4, festauler(N,T), allDiffQuadrats(T, N, CNF).

N = 4

CNF = [[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]

T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

```

### Crides internes

- [Subset](subset)
- [All Diff](all-diff)

## Subset

A partir de una matriu, extraurem el llistat de caselles S corresponent a el subquadrat Q demanat per paràmetre. Per fer-ho es sitúa un inici en funció del nombre del quadrat demanat seguint la fòrmula:

fila = (Q mod SQRT(N)) * SQRT(N)
columna = (Q / SQRT(N)) * SQRT(N)

UN cop calculats els inicis passem tant el tauler com els inicis a un predicat extreu que acaba extreient tots els elements que estan compresos entre els inicis i els inicis + SQRT(N).

### Predicat

```
%%%%%%%%%%%%%%%%%
% subset(T,N,Q,S)
% Serveix per extreure Un subquadrat amb el valor SUBQUADRAT.
% T és tauler
% N és el valor del sudoku.
% S Serà el subset referent al subquadrat de numero subquadrat.
% -----------------
% | 0 | 0 | 1 | 1 |
% | 0 | 0 | 1 | 1 |
% | 2 | 2 | 3 | 3 |
% | 2 | 2 | 3 | 3 |
% -----------------
% SUBQUADRAT pot prendre valor de 1 - N on en la matriu expressada anteriorment cada valor
% refereix al subquadrat marcat amb els nombres de 1 a 4.
subset(T,N,SUBQUADRAT,S):- P is truncate(sqrt(N)),
                           X is (SUBQUADRAT mod P)*P,
                           Y is (SUBQUADRAT div P)*P,
                           extreu(T,X,Y,N,S).
```

### Execució

Veiem la crida de subset per cada un dels quadrats en un sudoku N=4.

```
| ?- festauler(4,T), subset(T,4,0,SUBSET).
SUBSET = [[1,2,3,4],[5,6,7,8],[17,18,19,20],[21,22,23,24]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

| ?- festauler(4,T), subset(T,4,1,SUBSET).
SUBSET = [[9,10,11,12],[13,14,15,16],[25,26,27,28],[29,30,31,32]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

| ?- festauler(4,T), subset(T,4,2,SUBSET).
SUBSET = [[33,34,35,36],[37,38,39,40],[49,50,51,52],[53,54,55,56]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

| ?- festauler(4,T), subset(T,4,3,SUBSET).
SUBSET = [[41,42,43,44],[45,46,47,48],[57,58,59,60],[61,62,63,64]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

### Crides internes

- [Extreu](extreu)

## Extreu

L'objectiu de l'extreu és que donats dos inicis referents a la fila i la columna extregui els elements compressos entre els inicis i els inicis + SQRT(N) que son els que delimiten el quadrat.

Es realitza una verificació de tots els elements del tauler (tallant quan s'ha s'ha extret tot el quadrat) per cada quadrat que es vol extreure. Comprovant si la fila i la columna avaluats en aquest moment estan compresos dins del rang estipulat entre inici i final per files i columnes.

Un cop hem trobat tots els elements d'una fila, la fila es suprimeix i es segueix amb la cua que son la resta de files de la matriu. Si No s'ha trobat l'element encara es van passant elements i files fins arrivar a dins del rang. En cas que es trobi tots els elements de dins del quadrat i encara quedin files per mirar es talla.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%
% extreu(T,XI,YI,N,S)
% Extreu el subquadrat que estigui entre XI - XI + SQRT(N) i  YI - YI + SQRT(N) on X refereix a files i Y a columnes.
% T és Tauler
% XI és Fila inicial
% YI és columna inicial
% N és el valor del SUDOKU
% S serà el subquadrat de tamany SQRT(N) que està entre  XI - XI + SQRT(N) i  YI - YI + SQRT(N).
extreu(T,XI,YI,N,S):- ARR is truncate(sqrt(N)),
                      XF is XI + ARR, YF is YI+ARR,
                      iExtreu(T,XI,YI,XF,YF,0,0,S), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iExtreu(T,XI,YI,XF,YF,X,Y,S)
% Inmersió d'extreu. Retorna el subquadrat entre XI i XF, YI i YF.
% T és Tauler
% XI és Fila inici, XF és fila Final, X és Fila actual
% YI és columna inici, YF és columna Final, Y és columna actual
% S és el subquadrat format per tots els elements entre XI i XY, YI i YF.

% ja ha trobat tot el quadrat
iExtreu(_,_,_,_,YF,_,YF,[]):-!.
% Encara no he arribat a la fila que cerco.
iExtreu(T,XI,YI,XF,YF,_,Y,S):-    Y < YI, append([_],CUAF,T), YSEG is Y+1,
                                  iExtreu(CUAF,XI,YI,XF,YF,0,YSEG,S).
% me passat de columnes (X) puc saltar a la linia seguent
iExtreu(T,XI,YI,XF,YF,XF,Y,S):-   append([_],CUAF,T), YSEG is Y+1,
                                  iExtreu(CUAF,XI,YI,XF,YF,0,YSEG,S).
% No me passat ni de files ni de columnes i miro si estic dins del rang (extrec)
iExtreu(T,XI,YI,XF,YF,X,Y,S):-    append([PF],CUAF,T), append([PC], CUAC, PF),
                                  dinsRang(XI,XF,YI,YF,X,Y),
                                  append([CUAC],CUAF,MAT), XSEG is X+1,
                                  iExtreu(MAT,XI,YI,XF,YF,XSEG,Y,Q),
                                  append([PC],Q,S),!.
% Encara no he trobat l'inici del rang i vaig passant elements
iExtreu(T,XI,YI,XF,YF,X,Y,S):-    append([PF],CUAF,T),
                                  append([_], CUAC, PF),
                                  append([CUAC],CUAF,MAT),  X<XF, XS is X+1,
                                  iExtreu(MAT,XI,YI,XF,YF,XS,Y,S).
```

### Execució

```
| ?- festauler(4, T), extreu(T,2,2,4,QUADRAT).

QUADRAT = [[41,42,43,44],[45,46,47,48],[57,58,59,60],[61,62,63,64]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]
```

### Crides internes

- [Dins Rang](dins-rang)

## Dins Rang

Únicament comprova que x i y estan ditre de Xi i XF i Yi i Yf.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dinsRang(XI,XF,YI,YF,X,Y)
% Comprova que X i Y estan entre XI i XF, YI i YF els finals no inclosos.
% XI és Fila inici, XF és fila Final, X és Fila actual
% YI és columna inici, YF és columna Final, Y és columna actual
dinsRang(XI,XF,YI,YF,X,Y):-  X>=XI, X<XF, Y>=YI, Y<YF.
```

### Execució

```
| ?- dinsRang(0,2,0,2,1,1).

yes
| ?- dinsRang(0,2,0,2,3,3).

no
| ?-
```

### Crides internes

## Sat

Mira la satisfactibilitat de una CNF. utiltiza l'algoritme de simplificació de variables eliminant clàusules de dins de la CNF a mesura que va decidint literals. Cada literal que fixa el valor per aquell literal com a part d'un model. Llavors elimina totes les clàusules on apareix aquest literal, i quan es troba ell mateix negat, el treu de la clàusula. Va aplicant aquest algoritme fins que es queda sense clàusules i per tant completa el model.

Si entre el les clàusules restants es troba la clàusula buida és que s'ha trobat un contraexemple i que per tant no té solució per a la interpretació donada fins el moment. Llavors el prolog genera el backtracking per veure si en alguna altre branca de l'arbre de cerca la CNF és satisfactible.

### Predicat

```
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
```

### Execució

En el següent exemple d'execució li demanarem al Sat tots els possibles models entrant ';'. No considerem que sigui una bona idea tallar aquest sat per que així podem aconseguir diferents models donada una CNF que representa el sudoku.

```
| ?- taulerSudoku(4, [c(1,1,3),c(2,2,1),c(4,4,3)],T, C), codificaSudoku(4, T, C, CNF), sat(CNF, [], MODEL).
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,6,-8,-10,-14,-38,-54,56,-52,-60,-40,39,-18,20,-28,-32,30,-26,27,-36,-46,9,-12,-13,16,-41,-57,58,-50,49,-33,34,-42,44,-48,45]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]] ? ;
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,6,-8,-10,-14,-38,-54,56,-52,-60,-40,39,-18,20,-28,-32,30,-26,27,-36,-46,-9,12,-16,13,-44,-45,48,33,-34,-41,42,-49,50,-58,57]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]] ? ;
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,6,-8,-10,-14,-38,-54,56,-52,-60,-40,39,-18,20,-28,-32,30,-26,27,-36,-46,-9,12,-16,13,-44,-45,48,-33,34,-42,41,-50,49,-57,58]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]] ? ;
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,-6,8,-12,-16,-40,-56,54,-50,-58,-38,39,-20,18,-26,-30,32,-28,27,-34,-48,9,-10,-13,14,-41,-57,60,-52,49,-33,36,-44,42,-46,45]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]] ? ;
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,-6,8,-12,-16,-40,-56,54,-50,-58,-38,39,-20,18,-26,-30,32,-28,27,-34,-48,-9,10,-14,13,-42,-45,46,33,-36,-41,44,-49,52,-60,57]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]] ? ;
SAT!!, SUDOKU PISCINAS!!!

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
MODEL = [3,21,63,-1,-2,-4,-22,-23,-24,-61,-62,-64,-7,-11,-15,-17,-25,-29,-51,-55,-59,-19,-35,-5,-37,-53,-31,-47,-43,-6,8,-12,-16,-40,-56,54,-50,-58,-38,39,-20,18,-26,-30,32,-28,27,-34,-48,-9,10,-14,13,-42,-45,46,-33,36,-44,41,-52,49,-57,60]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

(156 ms) yes
```

### Crides internes

- [Decideix](decideix)
- [Simplif](simplif)

## Decideix

Si trobem una clàusula amb un sol literal, tirem pel dret amb aquest literal, altrament tirem pel dret amb el primer literal de la primera clàusula o el primer literal de la primera clàusula negat.

### Predicat

```
%%%%%%%%%%%%%%%%%%
% decideix(F, Lit)
% Donat una CNF,
% -> el segon parametre sera un literal de CNF
%  - si hi ha una clausula unitaria sera aquest literal, sino
%  - un qualsevol o el seu negat.
                % detectar clàusula unitària
decideix(L,A):- append(_,[X|_], L),
                append([A],[],X),!.
                        % Retorna el primer element
decideix([PL|_], A) :- append([A],_,PL).
                       % Retorna el negat del primer element
decideix([PL|_], B) :- append([A],_,PL), B is 0-A.
```

### Execució

```
| ?- decideix([[3],[21],[63],[1,2,3,4]], LITERAL).

LITERAL = 3

yes
| ?- decideix([[1,2,3,4]], LITERAL).

LITERAL = 1 ? ;
LITERAL = -1
```

## Simplif

Va treien totes les clàusules on hi aparegui el literl que li passem i va treient els negats d'aquest literal de dins de les clàusules.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%
% simplif(Lit, CNF, CNFS)
% Donat un literal Lit i una CNF,
% -> el tercer parametre sera la CNF que ens han donat simplificada:
%  - sense les clausules que tenen lit
%  - treient -Lit de les clausules on hi es, si apareix la clausula buida fallara.
simplif(_, [], []). % QUAN TINGUEM LA LLISTA BUIDA RETORNEM LA LLISTA BUIDA.
                                     % Comprovem si conté lit a primera
simplif(Lit, [PRIMERA|CUA], CNFS) :- member(Lit, PRIMERA),
                                     % seguim amb la cua.
                                     simplif(Lit, CUA, CNFS), !.
                                     % neguem el literal i el fotem a x
simplif(Lit, [PRIMERA|CUA], CNFS) :- X is 0-Lit,
                                     append(A, [X|XS], PRIMERA),
                                     % Treure el literal negat a primera serà res
                                     append(A, XS, RES),
                                     % crida recursiva
                                     simplif(Lit, CUA, CUACNFS),
                                     % Muntem la cadena final
                                     append([RES], CUACNFS, CNFS), !.
                                     % Crida recursiva quan no conté Lit
simplif(Lit, [PRIMERA|CUA], CNFS) :- simplif(Lit, CUA, CUACNFS),
                                     % Concatenar resultats
                                     append([PRIMERA], CUACNFS, CNFS), !.
```

### Execució

```
| ?- taulerSudoku(4, [c(1,1,3),c(2,2,1),c(4,4,3)],T, C), codificaSudoku(4, T, C, CNF), simplif(3, CNF, RESTA_CNF).

C = [[3],[21],[63]]
CNF = [[3],[21],[63],[1,2,3,4],[-1,-2],[-1,-3],[-1,-4],[-2,-3],[-2,-4],[-3,-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-3,-7],[-3,-11],[-3,-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-3,-19],[-3,-35],[-3,-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-3,-7],[-3,-19],[-3,-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
RESTA_CNF = [[21],[63],[-1,-2],[-1],[-1,-4],[-2],[-2,-4],[-4],[5,6,7,8],[-5,-6],[-5,-7],[-5,-8],[-6,-7],[-6,-8],[-7,-8],[9,10,11,12],[-9,-10],[-9,-11],[-9,-12],[-10,-11],[-10,-12],[-11,-12],[13,14,15,16],[-13,-14],[-13,-15],[-13,-16],[-14,-15],[-14,-16],[-15,-16],[17,18,19,20],[-17,-18],[-17,-19],[-17,-20],[-18,-19],[-18,-20],[-19,-20],[21,22,23,24],[-21,-22],[-21,-23],[-21,-24],[-22,-23],[-22,-24],[-23,-24],[25,26,27,28],[-25,-26],[-25,-27],[-25,-28],[-26,-27],[-26,-28],[-27,-28],[29,30,31,32],[-29,-30],[-29,-31],[-29,-32],[-30,-31],[-30,-32],[-31,-32],[33,34,35,36],[-33,-34],[-33,-35],[-33,-36],[-34,-35],[-34,-36],[-35,-36],[37,38,39,40],[-37,-38],[-37,-39],[-37,-40],[-38,-39],[-38,-40],[-39,-40],[41,42,43,44],[-41,-42],[-41,-43],[-41,-44],[-42,-43],[-42,-44],[-43,-44],[45,46,47,48],[-45,-46],[-45,-47],[-45,-48],[-46,-47],[-46,-48],[-47,-48],[49,50,51,52],[-49,-50],[-49,-51],[-49,-52],[-50,-51],[-50,-52],[-51,-52],[53,54,55,56],[-53,-54],[-53,-55],[-53,-56],[-54,-55],[-54,-56],[-55,-56],[57,58,59,60],[-57,-58],[-57,-59],[-57,-60],[-58,-59],[-58,-60],[-59,-60],[61,62,63,64],[-61,-62],[-61,-63],[-61,-64],[-62,-63],[-62,-64],[-63,-64],[-1,-5],[-1,-9],[-1,-13],[-5,-9],[-5,-13],[-9,-13],[-2,-6],[-2,-10],[-2,-14],[-6,-10],[-6,-14],[-10,-14],[-7],[-11],[-15],[-7,-11],[-7,-15],[-11,-15],[-4,-8],[-4,-12],[-4,-16],[-8,-12],[-8,-16],[-12,-16],[-17,-21],[-17,-25],[-17,-29],[-21,-25],[-21,-29],[-25,-29],[-18,-22],[-18,-26],[-18,-30],[-22,-26],[-22,-30],[-26,-30],[-19,-23],[-19,-27],[-19,-31],[-23,-27],[-23,-31],[-27,-31],[-20,-24],[-20,-28],[-20,-32],[-24,-28],[-24,-32],[-28,-32],[-33,-37],[-33,-41],[-33,-45],[-37,-41],[-37,-45],[-41,-45],[-34,-38],[-34,-42],[-34,-46],[-38,-42],[-38,-46],[-42,-46],[-35,-39],[-35,-43],[-35,-47],[-39,-43],[-39,-47],[-43,-47],[-36,-40],[-36,-44],[-36,-48],[-40,-44],[-40,-48],[-44,-48],[-49,-53],[-49,-57],[-49,-61],[-53,-57],[-53,-61],[-57,-61],[-50,-54],[-50,-58],[-50,-62],[-54,-58],[-54,-62],[-58,-62],[-51,-55],[-51,-59],[-51,-63],[-55,-59],[-55,-63],[-59,-63],[-52,-56],[-52,-60],[-52,-64],[-56,-60],[-56,-64],[-60,-64],[-1,-17],[-1,-33],[-1,-49],[-17,-33],[-17,-49],[-33,-49],[-2,-18],[-2,-34],[-2,-50],[-18,-34],[-18,-50],[-34,-50],[-19],[-35],[-51],[-19,-35],[-19,-51],[-35,-51],[-4,-20],[-4,-36],[-4,-52],[-20,-36],[-20,-52],[-36,-52],[-5,-21],[-5,-37],[-5,-53],[-21,-37],[-21,-53],[-37,-53],[-6,-22],[-6,-38],[-6,-54],[-22,-38],[-22,-54],[-38,-54],[-7,-23],[-7,-39],[-7,-55],[-23,-39],[-23,-55],[-39,-55],[-8,-24],[-8,-40],[-8,-56],[-24,-40],[-24,-56],[-40,-56],[-9,-25],[-9,-41],[-9,-57],[-25,-41],[-25,-57],[-41,-57],[-10,-26],[-10,-42],[-10,-58],[-26,-42],[-26,-58],[-42,-58],[-11,-27],[-11,-43],[-11,-59],[-27,-43],[-27,-59],[-43,-59],[-12,-28],[-12,-44],[-12,-60],[-28,-44],[-28,-60],[-44,-60],[-13,-29],[-13,-45],[-13,-61],[-29,-45],[-29,-61],[-45,-61],[-14,-30],[-14,-46],[-14,-62],[-30,-46],[-30,-62],[-46,-62],[-15,-31],[-15,-47],[-15,-63],[-31,-47],[-31,-63],[-47,-63],[-16,-32],[-16,-48],[-16,-64],[-32,-48],[-32,-64],[-48,-64],[-1,-5],[-1,-17],[-1,-21],[-5,-17],[-5,-21],[-17,-21],[-2,-6],[-2,-18],[-2,-22],[-6,-18],[-6,-22],[-18,-22],[-7],[-19],[-23],[-7,-19],[-7,-23],[-19,-23],[-4,-8],[-4,-20],[-4,-24],[-8,-20],[-8,-24],[-20,-24],[-9,-13],[-9,-25],[-9,-29],[-13,-25],[-13,-29],[-25,-29],[-10,-14],[-10,-26],[-10,-30],[-14,-26],[-14,-30],[-26,-30],[-11,-15],[-11,-27],[-11,-31],[-15,-27],[-15,-31],[-27,-31],[-12,-16],[-12,-28],[-12,-32],[-16,-28],[-16,-32],[-28,-32],[-33,-37],[-33,-49],[-33,-53],[-37,-49],[-37,-53],[-49,-53],[-34,-38],[-34,-50],[-34,-54],[-38,-50],[-38,-54],[-50,-54],[-35,-39],[-35,-51],[-35,-55],[-39,-51],[-39,-55],[-51,-55],[-36,-40],[-36,-52],[-36,-56],[-40,-52],[-40,-56],[-52,-56],[-41,-45],[-41,-57],[-41,-61],[-45,-57],[-45,-61],[-57,-61],[-42,-46],[-42,-58],[-42,-62],[-46,-58],[-46,-62],[-58,-62],[-43,-47],[-43,-59],[-43,-63],[-47,-59],[-47,-63],[-59,-63],[-44,-48],[-44,-60],[-44,-64],[-48,-60],[-48,-64],[-60,-64]]
T = [[[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],[[17,18,19,20],[21,22,23,24],[25,26,27,28],[29,30,31,32]],[[33,34,35,36],[37,38,39,40],[41,42,43,44],[45,46,47,48]],[[49,50,51,52],[53,54,55,56],[57,58,59,60],[61,62,63,64]]]

```

## Mostra Sudoku

Mostra un sudoku ja sigui complert o incomplert a partir d'una entrada per teclat amb el format [c(F1,C1,V1)..c(Fn,Cn,Vn)].

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%
% mostraSudoku(N, IN)
% Mostra un sudoku expressat en format llistat de c(F,C,V) per pantalla.
% N és el tamany del sudoku
% IN és el sudoku representat en llistat de c(F,C,V).
mostraSudoku(N, IN) :- pintaSeparador(N), nl, iMostraSudoku(N,1,1,IN).

iMostraSudoku(N, F, _, _):-  F>N, !.
iMostraSudoku(N, F, C, IN):- C > N, write('|'), nl, pintaSeparador(N), nl,
                             FSEG is F+1, iMostraSudoku(N, FSEG, 1, IN).
iMostraSudoku(N, F, C, IN):- pintaCasella(F,C,IN), CSEG is C+1,
                             iMostraSudoku(N, F, CSEG, IN), !.
iMostraSudoku(N, F, C, IN):- CSEG is C+1, write('| '),
                             iMostraSudoku(N, F, CSEG, IN).
```

### Execució

```
| ?- mostraSudoku(4, [c(1,1,3),c(2,2,1),c(4,4,3)]).
---------
|3| | | |
---------
| |1| | |
---------
| | | | |
---------
| | | |3|
---------
```

### Crides internes

- [Pinta Separador](pinta-separador)
- [Pinta casella](pinta-casella)

## Pinta Separador

Pinta (N+2)+(N-1) guions com una sola linia.

### Predicat

```
%%%%%%%%%%%%%%%%%%%
% pintaSeparador(N)
% Pinta un separador generat amb '-' amb tamany N+2+N-1 o N*2+1 caràcters
pintaSeparador(N):- NF is (N+2)+(N-1), iPintaSeparador(1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%
% iPintaSeparador(N, NF)
% Crida inmersiva del pintaSeparador.
iPintaSeparador(N, NF):- N>NF, !.
iPintaSeparador(N, NF):- write('-'), NSEG is N+1, iPintaSeparador(NSEG, NF).
```

### Execució

```
| ?- pintaSeparador(4).
---------
```

## Pinta Casella

Cerca dins de la llista in si hi ha una casella que coincideixi amb files i columnes a partir del l'append i si hi és el pinta. Si no hi és retorna no.

### Predicat

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pintaCasella(F,C,IN, FCUA)
% Pinta el valor de una casella amb el separador al davant si coincideixen F = IN[F] i C = IN[C].
% F és el valor de la fila, C és el valor de la Columna
% IN son les caselles possibles a pintar per la casella [F,C]
% FCUA seran les caselles restants si es compleix que es pot pintar.
pintaCasella(F,C,IN) :- append(_,[c(F,C,V)|_], IN), write('|'), write(V), !.
```

### Execució

```
| ?- pintaCasella(1,1,[c(1,1,3),c(2,2,1),c(4,4,3)]).
|3

yes

| ?- pintaCasella(1,2,[c(1,1,3),c(2,2,1),c(4,4,3)]).

no  
```

# Exemples i jocs de proves.



# Comentaris sobre la solució i aspectes positius.

## Abstracció

Com és d'esperar, en les nostres pràctiques mirem d'abstreure les funcionalitats per reutilitzar-les en diferents parts del programa. Per exemple podem observar les anbstraccions dels predicats [montaParelles](monta-parelles) o del [matTransposa](mat-transposa) i fins i tot del [allDiff](all-diff) entre altres.

## Talls.

S'ha procurat que el prolog faci la feina que ha de fer i cap altre. Per demostrar-ho s'ha realitzat un exemple d'execució de cada predicat per que e pugui veure la solució que es proposa i juntament amb la definició del predicat es pugui interpretar que no hi ha més opcions possibles.

S'ha de descartar el tall en el sat, ja que el sat interessa que torni models si el cridem independentment de la resta de predicats.

## L'us de l'append

L'append és el més utilitzat per el tractament de llistes en aquesta pràctica.

### Cerques

L'ús de l'append per fer una cerca en un llistat per exemple de caselles (Cerca si hi ha la casella que coincideix en files i columnes a dins de IN i la pinta):

```
pintaCasella(F,C,IN) :- append(_,[c(F,C,V)|_], IN), write('|'), write(V), !.
```

### Detectar clàusula unitària

Podem usar-la per detectar si hi ha una llista amb un sol element dins d'una llista.

```
decideix(L,A):- append(_,[X|_], L), append([A],[],X),!.
```

### Construir i desconstruir Llistes

És molt usat en general per poder desengranar les llistes a plaer. També per construir-les.

```
combina(L, COMBINAT):- append([A,B], CUA, L),
          					   append([A],CUA,PIVOTA),
          					   combina(PIVOTA,ITSEG),
                       append([[A,B]],ITSEG,COMBINAT),!.
```

# Conclusió

La pràctica de prolog ha sigut molt clara i fàcil d'interpretar un cop estudiat l'esquelet de la pràctica. Reconeixem que sense l'esquelet ens hi hauríem fet bastant de mal. Ens ha agradat la manera que té el prolog de treballar, com evalúa i genera l'arbre de cerca i com realitza el backtracking ell solet. És quelcom curiós i amb aplicació directe a problemes que son molt interessants.
