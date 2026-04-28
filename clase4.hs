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


esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                            then True
                            else tieneInterpretacion x ys

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var "foo"

--Funcion auxiliar que da un estado a una clausula unitaria
darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p,True)]
darValor [Not (Var p)] = [(p,False)]

--Funcion auxiliar para acumular las clausulas que no son unitarias
acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_,xs) (l2, ys) = (l2, xs ++ ys)


--Funcion auxiliar para acumular las literales del modelo
acumularModelo :: Estado -> Estado -> Estado
acumularModelo (m1,_) (m2, ys) = (m1 ++ m2, ys)