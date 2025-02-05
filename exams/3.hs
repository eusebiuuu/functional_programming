-- link: https://github.com/FMI-Materials/FMI-Materials/blob/main/Year%20II/Semester%20I/Programare%20Functionala/Modele%20Examen/2023%20-%202024/Subiect%2004.jpg

-- Ex 2

data Pereche a b = MyP a b deriving Show

data Lista a = MyL [a] deriving Show

class MyOp m where
    myFilter :: (a -> Bool) -> (b -> Bool) -> m (Pereche a b) -> m (Pereche a b)

instance MyOp Lista where
    myFilter _ _ (MyL []) = MyL []
    myFilter f1 f2 (MyL list) = MyL (filter (\(MyP x y) -> (f1 x) && (f2 y)) list)

lp :: Lista (Pereche Int Char)
lp = MyL [MyP 97 'a', MyP 3 'b', MyP 100 'd']


