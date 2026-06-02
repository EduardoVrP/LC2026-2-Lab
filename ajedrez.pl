% Posiciones: l1, l2, l3 (izquierda), r1, r2, r3 (derecha)
% Piezas: peon, alfil, reina, rey, torre, caballo

% Solución: lista de (Posición, Pieza)
solucion(Tablero) :-
    % Quitamos el peón de la lista, lo fijamos en r3
    Piezas = [alfil, torre, caballo, rey, reina],
    
    % Vemos permutaciones para que no se vayan a repetir las piezas en la lista del resultado
    permutacion(Piezas, [P1, P2, P3, P4, P5]),

    Tablero = [ (l1, P1), (l2, P2), (l3, P3),
                (r1, P4), (r2, P5), (r3, peon) ],


    % 1. La torre está cerca del caballo, pero no encara a la reina
    cerca_del_caballo(Tablero, torre),
    \+ encara(Tablero, torre, reina),

    % 2. El rey no está al lado de la reina, pero encara al caballo, justo en la orilla
    \+ al_lado(Tablero, rey, reina),
    encara(Tablero, rey, caballo),
    en_orilla(Tablero, rey),

    % 3. El caballo no obedece a su marca
    % Esto implica que no está en l1
    \+ member((l1, caballo), Tablero).
    

% Reglas auxiliares

% Dos piezas están "al lado" si están en posiciones adyacentes del mismo lado
al_lado(Tablero, P1, P2) :-
    member((Pos1, P1), Tablero),
    member((Pos2, P2), Tablero),
    lado(Pos1, Lado), lado(Pos2, Lado),
    adyacente(Pos1, Pos2).

% Encara: están en la misma fila, una en izquierda y otra en derecha
encara(Tablero, P1, P2) :-
    member((Pos1, P1), Tablero),
    member((Pos2, P2), Tablero),
    encaradas(Pos1, Pos2).

% Cerca del caballo: casillas contiguas en el mismo lado
cerca_del_caballo(Tablero, Pieza) :-
    member((Pos1, Pieza), Tablero),
    member((Pos2, caballo), Tablero),
    lado(Pos1, Lado), lado(Pos2, Lado),
    adyacente(Pos1, Pos2).

% En orilla: posiciones l1, l3, r1, r3
en_orilla(Tablero, Pieza) :-
    member((Pos, Pieza), Tablero),
    member(Pos, [l1, l3, r1, r3]).

% Definiciones de apoyo
lado(l1, izquierda).
lado(l2, izquierda). 
lado(l3, izquierda).
lado(r1, derecha).   
lado(r2, derecha).   
lado(r3, derecha).

adyacente(l1, l2). 
adyacente(l2, l1).
adyacente(l2, l3). 
adyacente(l3, l2).
adyacente(r1, r2). 
adyacente(r2, r1).
adyacente(r2, r3). 
adyacente(r3, r2).

encaradas(l1, r1). 
encaradas(r1, l1).
encaradas(l2, r2). 
encaradas(r2, l2).
encaradas(l3, r3). 
encaradas(r3, l3).

%Para ver si una lista es permutacion de otra lista
permutacion([], []).
permutacion(L, [X|Xs]) :-
    quitar(X, L, R),
    permutacion(R, Xs).

quitar(X, [X|Xs], Xs).
quitar(X, [Y|Ys], [Y|Zs]) :-
    quitar(X, Ys, Zs).