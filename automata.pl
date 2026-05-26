estadoinicial(q0).
estadofinal(q4). 

delta(q0,a,q0). 
delta(q0,b,q1).
delta(q1,c,q2).
delta(q1,d,q3).
delta(q2,d,q2).
delta(q2,f,q4).
delta(q3,e,q3).
delta(q3,f,q4).

reconoce(Lista) :- deltaEstrella(Lista, q0).

deltaEstrella([],EstadoI) :- estadofinal(EstadoI).
deltaEstrella([X],EstadoI) :- delta(EstadoI,X,EstadoTrans), estadofinal(EstadoTrans).
deltaEstrella([X | Resto],EstadoI) :- delta(EstadoI, X, EstadoTrans), deltaEstrella(Resto,EstadoTrans).