import Data.Ratio

data Z a = Z a a

instance Num a => Num (Z a) where
    Z a b + Z c d = Z (a+c) (b+d)
    Z a b * Z c d = Z (a*c-5*b*d) (a*d+b*c)
    negate (Z a b) = Z (negate a) (negate b)
    fromInteger a = Z (fromInteger a) 0
    abs _ = error "no abs"
    signum _ = error "no signum"

instance Functor Z where
    fmap f (Z a b) = Z (f a) (f b)

instance Show a => Show (Z a) where
    show (Z a b) = "(" ++ show a ++ "+" ++ show b ++ "√-5" ++ ")"

norm (Z a b) = a*a+5*b*b

det a b c d = a*d-b*c

disc :: Num a => Z a -> Z a -> a
disc (Z a b) (Z c d) = -20*(b*c-a*d)^2

-- Write Z u v in terms of basis {Z a b, Z c d}
c (Z a b) (Z c d) (Z u v) =
    ((d*u-b*v)/(a*d-b*c), (-b*u+a*v)/(a*d-b*c))

main = do
    let a = Z 1 1
    let b = Z 1 2
    print (a*b)

    print $ disc a b
    print $ c (fmap fromInteger a) (fmap fromInteger b) (Z 1 (0::Rational))
    print $ c (fmap fromInteger a) (fmap fromInteger b) (Z 0 (1::Rational))

    print $ norm a
    print $ norm b
    print $ norm (a*b)

    print $ norm (Z 2 0)
    print $ norm (Z 1 1)
