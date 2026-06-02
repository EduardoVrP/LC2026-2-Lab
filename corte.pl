color(rojo).
color(verde).
color(azul).

elige(X) :- color(X),!.

padre(juan,pedro).
padre(juan,maria).

gusta(pedro,futbol).
gusta(pedro,ajedrez).
gusta(maria,tenis).

hijo_con_hobby(Padre,Hijo,Hobby) :-
    padre(Padre,Hijo),
    !,
    gusta(Hijo,Hobby).


delall(_,[],[]).
delall(N,[N | Resto],Nueva) :- !,delall(N,Resto,Nueva).
delall(N, [X| Resto], [X | Nueva]) :- delall(N,Resto,Nueva).