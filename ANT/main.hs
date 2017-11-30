import Data.Monoid

data Monomial a = M a [Int] deriving Show

zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault _ _ _ [] [] = []
zipWithDefault f a b [] (x:xs) = f a x : zipWithDefault f a b [] xs
zipWithDefault f a b (x:xs) [] = f x b : zipWithDefault f a b xs []
zipWithDefault f a b (x:xs) (y:ys) = f x y : zipWithDefault f a b xs ys

instance Num a => Monoid (Monomial a) where
    mempty = M 1 []
    M a xs `mappend` M b ys = M (a*b) $ zipWithDefault (+) 0 0 xs ys

newtype Polynomial a = P [Monomial a] deriving Show

instance Num a => Monoid (Polynomial a) where
    


main = do
    let a = M 1 [1, 1, 1]
    let b = M 2 [0, 0, 3, 3]
    print $ a <> b
    print "Hello"
