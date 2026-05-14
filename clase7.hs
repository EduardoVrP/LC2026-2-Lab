type Nombre = String

--Terminos de la logica de primer orden
--Fun recibe los argumentos con una lista de terminos
data Term = Var Nombre | Fun Nombre [Term] deriving (Eq)

instance Show Term where 
    show (Var p) = p
    show (Fun a []) = a
    show (Fun f args) = f ++ "(" ++ showArgs args ++ ")"

-- función auxiliar
showArgs :: [Term] -> String
showArgs [] = ""
showArgs [x] = show x
showArgs (x:xs) = show x ++ "," ++ showArgs xs

--Sinonimo para las sustituciones
type Subst = [(Nombre,Term)]



--Funcion auxiliar que levanta una bandera si hay sustitucion por hacer e indica la sustitucion
haySustitucion :: Term -> Subst -> (Bool, Term)
haySustitucion _ [] = (False, Var "foo")
haySustitucion (Var x) ((y, term):resto) = if x == y
                                        then (True,term)
                                        else haySustitucion (Var x) resto  
haySustitucion _ _ = (False, Var "foo")


--Aplicar una sustitucion a una variable
apsubT :: Term -> Subst -> Term
apsubT (Var x) sigma = if bandera
                        then term
                        else (Var x)
                where
                    indicador = haySustitucion (Var x) sigma
                    bandera = fst indicador
                    term = snd indicador