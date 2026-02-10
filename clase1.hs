--Comentario de una linea

{-
Comentario de varias lineas
-}

--Todo es una funcion en el sentido matemático
cuadratica :: Int -> Int
cuadratica x = x^2

--Incluso las constantes, son funciones que no reciben nada y siempre devuelven lo mismo
constante :: Int
constante = 10

--Si necesito mas agumentos
multiplicacion :: Int -> Int -> Int
multiplicacion a b = a*b

--Ejemplos sencillos
suma2 :: Int -> Int
suma2 x = 2 + x

esParChafa :: Int -> Bool
esParChafa x = x `mod` 2 == 0

esPar :: Int -> Bool
esPar 0 = True
esPar x = esImpar (x-1)

esImpar :: Int -> Bool
esImpar 0 = False
esImpar x = esPar (x-1)

--LISTAS 
--[] es una lista de elementos de A
--si xs es una lista y x un elemento de A, entonces x:xs es una lista de elementos de A

--[1,2,3,4,5,6] es una lista
--[1,2..10] lista de numeros del 1 al 10

longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

alternar :: [a] -> [a] -> [a]
alternar [] [] = []
alternar (x:xs) (y:ys) = x:y:(alternar xs ys) 


--Funcion que agrega un elemento al final de una lista dada con cosas del mismo tipo
agregarFinal :: a -> [a] -> [a]
agregarFinal x xs = xs ++ [x]

concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x:(concatena xs ys)

--Listas por comprension
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano xs ys  = [(x,y) | x <- xs, y <- ys]


--quicksort
quicksort ::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

--Sinonimos
type ListaNumeros = [Integer]
type Matriz = [[Int]]

--Tipos de datos algebraicos
data Natural = Cero | Succ Natural

data Lista a = Empty | Cons a (Lista a)

long2 :: Lista a -> Integer
long2 Empty = 0
long2 (Cons x xs) = 1 + long2 xs

data Shape = Square Float 
            | Rectangle Float Float

area :: Shape -> Float
area (Square x) = x ** 2
area (Rectangle x y) = x * y

data Persona = Persona {nombre :: String,
                        apellido :: String,
                        estatura :: Float,
                        peso :: Float,
                        edad :: Int} deriving Show

--"Variable global"
juan = (Persona {nombre = "Juan",apellido = "Sanchez",estatura = 1.72, peso = 68.5, edad = 22})
juancho = (Persona {nombre = "Juan",apellido = "Cordova",estatura = 1.63, peso = 62, edad = 25})
haskell = (Persona {nombre = "Haskell", apellido = "Perez",estatura = 1.50, peso = 52.3, edad = 18})

imc :: Persona -> Float
imc persona = peso persona / (estatura persona ** 2)

--if y where
mayorDeEdad :: Persona -> String
mayorDeEdad persona = if anios >= 18 then "Es mayor de edad" else "No es mayor de edad"
                        where anios = edad persona

--Casos
dummy :: Float -> Float
dummy x
    | x < 5 = x + 2
    | x <= 15 = sqrt x
    | otherwise = x

-- Hacer que Persona se pueda ordenar por altura
instance Ord Persona where
    compare p1 p2 = compare (estatura p1) (estatura p2)

-- Hacer que Persona se pueda comparar, 2 personas son iguales si se llaman igual
instance Eq Persona where
    p1 == p2 = nombre p1 == nombre p2 --"p1 y p2 son iguales si y solo si se llaman igual"