--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])

data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void

--Funcion para devolver el segundo elemento de una tupla
segundoElemento :: (a, b) -> b
segundoElemento (_, y) = y


--Funcion para construir el arbol dpll desde un estado dado
--Para esto, se supone que ya tienen implementadas las funciones unit,red,sep,elim y heuristicsLiteral
construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado 
    | conflict estado = Node estado Void
    | success estado = Node estado Void
    | (segundoElemento propuesto) /= (segundoElemento estado) = Node estado (construirArbolDPLL propuesto)
    | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
    where
        propuesto = red (elim (unit (estado))) --Tratamos de aplicar unit, elim y/o red. Si hay cambios en las clausulas, se aplico alguna de las reglas.
        literal = heuristicsLiteral (segundoElemento estado) --Sacamos una literal
        (izq,der) = sep literal estado --Aplicamos separacion
