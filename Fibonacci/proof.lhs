> {-# OPTIONS_GHC -fno-warn-missing-methods #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> import qualified Data.Map.Strict as M
> import Data.Ratio
> import Data.Monoid
> import Data.Group
> import Control.Applicative

> fibonacci n = (α^^n-β^^n)/(α-β)
>               where α = ϕ
>                     β = -1/ϕ

> main = do
>     print $ map fibonacci [0..10]

> data Q = Q Rational Rational deriving Eq

> instance Show Q where
>     show (Q a b) = show a ++ (if b == 0 then "" else "+" ++ show b ++ "ϕ")

> instance Num Q where
>     fromInteger a = Q (fromInteger a) 0
>     Q a b + Q a' b' = Q (a+a') (b+b')
>     Q a b * Q a' b' = Q (a*a'+b*b') (a*b'+a'*b+b*b')
>     negate (Q a b) = Q (negate a) (negate b)

> instance Fractional Q where
>     recip (Q a b) =
>         let denominator = a*a+a*b-b*b
>         in Q ((a+b)/denominator) (-b/denominator)

> ϕ = Q 0 1
> α = ϕ
> β = 1-α

> sqrt5 = 2*ϕ-1

Integers modulo 2

> newtype Z2 = Z2 { unZ2 :: Int } deriving (Ord, Num, Eq, Show)

> instance Monoid Z2 where
>     mempty = 0
>     mappend (Z2 a) (Z2 b) = Z2 $ (a + b) `mod` 2

> instance Group Z2 where
>     invert = id

> instance Abelian Z2

> instance Monoid Int where
>     mempty = 0
>     mappend = (+)

> instance Group Int where
>     invert = negate

> instance Abelian Int

> type Expr a b c = GroupRing a (b, c)

> i :: Monoid g => r -> GroupRing r g
> i r = GR [(mempty, r)]

> j :: Num r => g -> GroupRing r g
> j g = GR [(g, 1)]

> newtype GroupRing r g = GR [(g, r)] deriving Show

> reduce' :: (Num r, Eq r, Ord g) => [(g, r)] -> [(g, r)]
> reduce' ts = filter (\(g, r) -> r /= 0) $ M.toList $ M.fromListWith (+) $ ts

> instance (Num r, Eq r, Group g, Ord g) => Num (GroupRing r g) where
>     fromInteger a = GR $ reduce' $ [(mempty, fromInteger a)]
>     GR as+GR bs = GR $ reduce' $ as ++ bs
>     GR as*GR bs = GR $ reduce' $ m <$> as <*> bs
>                     where m (g, r) (g', r') = (g <> g', r*r')
>     negate (GR as) = GR [(g, -r) | (g, r) <- as]

> ϕn, αn, βn, nn :: (Z2, Int)
> ϕn = (0, 1)
> αn = (0, 1)
> βn = (1, -1)
> nn = (1, 0)

Expression for F(n)

> fib = (j ϕn-j βn)*i (1/sqrt5)

Expression for Fibonacci number F(an+b)

> fib' a b = (i (α^^b)*(j $ ϕn `pow` a)-i (β^^b)*(j $ βn `pow` a))*i (1/sqrt5)
> lucas' a b = (i (α^^b)*(j $ ϕn `pow` a)+i (β^^b)*(j $ βn `pow` a))

> f 0 = 0
> f 1 = 1
> f n = f (n-1)+f (n-2)

> ex1 :: GroupRing Q (Z2, Int)
> ex1 = fib' 1 (-1)^2*fib' 1 1^2-fib' 1 (-2)^2*fib' 1 2^2-4*j nn*fib' 1 0^2

> newtype V a = V [a] deriving (Eq, Show, Ord, Functor)

> instance (Eq a, Monoid a) => Monoid (V a) where
>     mempty = V []
>     mappend (V a) (V b) = V $ trim (a `vmappend` b) where
>       vmappend (a:as) (b:bs) = (a <> b : vmappend as bs)
>       vmappend a [] = a
>       vmappend [] b = b
>       trim as | all (== mempty) as = []
>       trim (a : as) = a : trim as

> instance (Eq a, Group a) => Group (V a) where
>     invert (V as) = V $ map invert as

> instance (Eq a, Abelian a) => Abelian (V a) 

> (!) :: Monoid a => V a -> Int -> a
> V [] ! i = mempty
> V (a : _) ! 0 = a
> V (a : as) ! i = (!) (V as) (i-1)

> ϕi, αi, βi, ni :: (V Z2, V Int)
> ϕi = (V [0], V [1])
> αi = (V [0], V [1])
> βi = (V [1], V [-1])
> ni = (V [1], V [])

> ϕj, αj, βj, nj :: (V Z2, V Int)
> ϕj = (V [0, 0], V [0, 1])
> αj = (V [0, 0], V [0, 1])
> βj = (V [0, 1], V [0, -1])
> nj = (V [0, 1], V [])

> αpow :: Int -> Int -> Expr Q (V Z2) (V Int)
> αpow a b = j $ (αi `pow` a) <> (αj `pow` b)

Expression for Fibonacci number F(a*i+b*j+c)

> fib'' a b c = (i (α^^c)*(αpow a b)-i (β^^c)*(αpow a b))*i (1/sqrt5)
> lucas'' a b c = (i (α^^c)*(αpow a b)+i (β^^c)*(αpow a b))

> dot :: Num a => V a -> V a -> a
> dot (V a) (V b) = sum $ zipWith (*) a b

> evalTerm' :: Int -> Int -> ((V Z2, V Int), Q) -> Q
> evalTerm' i j ((b, c), a) = a * (-1)^^dot ij (fmap unZ2 b) * ϕ^^dot ij c
>   where ij = V [i, j]

> evalExpr' :: Int -> Int -> Expr Q (V Z2) (V Int) -> Q
> evalExpr' i j (GR ts) = sum (map (evalTerm' i j) ts)

> ex2 = fib'' 1 1 0+j nj*fib'' 1 (-1) 0 - fib'' 1 0 0*lucas'' 0 1 0
> ex3 = 2*fib'' 1 1 0-lucas'' 1 0 0*fib'' 0 1 0-lucas'' 0 1 0*fib'' 1 0 0

F(n)²F(m+1)²F(m-1)²-F(m)²F(n+1)²F(n-1)+(-1)ⁿF(m+n)F(m-n) = 0

> ex4 = fib'' 0 1 0^2*fib'' 1 0 1*fib'' 1 0 (-1)-fib'' 1 0 0^2*fib'' 0 1 1*fib'' 0 1 (-1)+j nj*fib'' 1 1 0*fib'' 1 (-1) 0

http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormulae.html
