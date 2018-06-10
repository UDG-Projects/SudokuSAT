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

## Índex :


## Propietats Sudoku

Com a propietats importants d'un sudoku tenim :

- Només podem fer sudokus de tamany N on N és un nombre que té una arrel entera. Per tant es poden fer sudokus de N=4, N=9, N=16...


## Tauler Sudoku

És un predicat que s'encarrega de generar un tauler a partir d'un nombre i una sèrie de inputs.

### Predicat :

```
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DONE MACARRONE!!
% taulerSudoku(N,C,Tauler,CNF)
% Donat el domini de les caselles (N), una inicialitzacio del Sudoku (de forma [c[1,2,5],...]),
% -> Tauler serà Llista de llistes de N-dominis del tauler,
% -> i CNF codifica els valors assignats amb clausules unitaries que forcen valor a les caselles inicialitzades
taulerSudoku(N,C,Tauler,CNF):- festauler(N,Tauler), inicialitzar(N,C,CNF).
```

### Execució :

  taulerSudoku(4, [c(1,1,3), c(2,2,1)],c(4,4,3), T, CNF).
