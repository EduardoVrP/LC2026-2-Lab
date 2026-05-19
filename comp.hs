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


--Funcion que calcula la composicion eliminando las repetidas
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = parte1 ++ s2
  where
    parte1 = [ (x, apsubT t s2) | (x,t) <- s1 ]


--Funcion auxiliar que realiza la composicion de una lista de sustituciones
compSusLista :: [Subst] -> Subst
compSusLista [] = []
compSusLista [s1] = s1
compSusLista (s1:s2:xs) = compSusLista ((compSus s1 s2):xs)

--Funcion auxiliar que verifica si un nombre figura en otro termino
figuraEn :: Nombre -> Term -> Bool
figuraEn x (Var y) = x == y
figuraEn x (Fun f args) = figuraEnAux x args

--Función auxiliar para verificar si un nombre figura en una lista de terminos
figuraEnAux :: Nombre -> [Term] -> Bool
figuraEnAux _ [] = False
figuraEnAux x (y:ys) = figuraEn x y || figuraEnAux x ys 
