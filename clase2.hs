--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- Contar variables en una formula
contarVariables :: Prop -> Int
contarVariables (Var _) = 1
contarVariables (Cons _) = 0
contarVariables (Not p) = contarVariables p
contarVariables (And p q) = contarVariables p + contarVariables q
contarVariables (Or p q) = contarVariables p + contarVariables q
contarVariables (Impl p q) = contarVariables p + contarVariables q
contarVariables (Syss p q) = contarVariables p + contarVariables q

-- Sustitución de variables
sustituir :: String -> Prop -> Prop -> Prop
sustituir x sub (Var y) = if x == y then sub else Var y
sustituir _ _ (Cons b) = Cons b
sustituir x sub (Not p) = Not (sustituir x sub p)
sustituir x sub (And p q) = And (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Or p q) = Or (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Impl p q) = Impl (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Syss p q) = Syss (sustituir x sub p) (sustituir x sub q)