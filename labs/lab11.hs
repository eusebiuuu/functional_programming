{-
class Functor f where
fmap :: ( a -> b ) -> f a -> f b
-}


data Constant a b = Constant b

instance Functor (Constant a) where
    fmap f (Constant x) = Constant (f x)


data Three a b c = Three a b c

data Three' a b = Three' a b b


data Four a b c d = Four a b c d

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b1) = Four'' a1 a2 a3 (f b1)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance (Show a) => Show (TalkToMe a) where
    show (Read g) = show (g "heey")

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print s t) = Print s (f t)
    fmap f (Read g) = Read (f . g)
dummy :: String -> Int
dummy str = length (str)
-- main :: IO ()
-- main = do
--     let var = Read dummy
--     let new_var = fmap (* 2) var
--     let str_to_print = show new_var
--     print str_to_print



data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

instance (Show a, Show b) => Show (Quant a b) where
    show (Bloor s) = "bloor " ++ show s
    show (Desk n) = "desk " ++ show n
    show (Finance) = "finance "

main :: IO ()
main = do
    let varD = Desk 6
    let varB = Bloor "hey" :: Quant [Four'' Int Float] String
    let new_bloor = fmap (++ " yoo") varB
    let str_to_print = fmap show new_bloor
    print str_to_print