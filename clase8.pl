% Hechos
mamifero(perro).
mamifero(gato).
ave(aguila).
ave(pinguino).
pez(tiburon).
no_vuela(pinguino).

%Hechos
padre(juan, maria).
padre(juan,carlos).
padre(juan,paco).
padre(carlos,monserrat).
padre(david,liz).
padre(carlos,ana).
madre(liz,monserrat).
madre(maria, ana).

%Regla 
abuelo(X,Y) :- padre(X,Z),padre(Z,Y).
abuelo(X,Y) :- padre(X,Z),madre(Z,Y).

%Hechos
edad(juan, 25).
edad(eduardo,25).
edad(maria, 19).
edad(pedro, 16).


mayor_edad(X) :- edad(X,N), N >= 18.

%El caso base es un hecho, el recursivo es una regla
miembro(X, [X|_]).
miembro(X, [_|T]) :- miembro(X, T).

longitud([],0).
longitud([_|T],N) :- longitud(T,N1), N is N1+1.