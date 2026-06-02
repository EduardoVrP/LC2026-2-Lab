arriba(a1).
arriba(a2).
arriba(a3).

abajo(b1).
abajo(b2).
abajo(b3).

encaradas(a1,b1).
encaradas(b1,a1).
encaradas(a2,b2).
encaradas(b2,a2).
encaradas(a3,b3).
encaradas(b3,a3).

orilla(a1).
orilla(a3).
orilla(b1).
orilla(b3).

al_lado(a1,a2).
al_lado(a2,a1).
al_lado(a2,a3).
al_lado(a3,a2).
al_lado(b1,b2).
al_lado(b2,b1).
al_lado(b2,b3).
al_lado(b3,b2).


% Encara: están en la misma fila, una en izquierda y otra en derecha
encara(Tablero, P1, P2) :-
    member((Pos1, P1), Tablero),
    member((Pos2, P2), Tablero),
    encaradas(Pos1, Pos2).

esta_cerca(Tablero, P1, P2) :- 
    member((Pos1, P1), Tablero),
    member((Pos2, P2), Tablero),
    al_lado(Pos1, Pos2).

esta_orilla(Tablero, P1) :- 
    member((Pos1, P1), Tablero),
    orilla(Pos1).

solucion(Res) :-
    %Piezas a usar
    Piezas = [rey,reina,alfil,caballo,torre],

    %Representacion del tablero
    Res = [(a1,P1),(a2,P2),(a3,P3),(b1,peon),(b2,P4),(b3,P5)],

    %De donde tomo las piezas a acomodar
    member(P1,Piezas),member(P2,Piezas),
    member(P3,Piezas),member(P4,Piezas),
    member(P5,Piezas),

    %No hay piezas repetidas
    P1 \= P2, P1 \= P3, P1 \= P4, P1 \= P5,
    P2 \= P3, P2 \= P4, P2 \= P5,
    P3 \= P4, P3 \= P5,
    P4 \= P5,

    %El caballo no obedece a su marca
    P3 \= caballo,

    %La torre esta cerca del caballo, pero no encara a la reina
    \+ encara(Res,torre,reina),
    esta_cerca(Res,torre,caballo),

    %El rey no está al lado de la reina, pero encara al caballo, justo en la orilla.
    \+ esta_cerca(Res, rey, reina),
    encara(Res, rey,caballo),
    esta_orilla(Res,rey).
