{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Monoid
import Data.Group
--import Data.Semigroup
import Control.Applicative

fibonacci n = (α^^n-β^^n)/(α-β)
              where α = ϕ
                    β = -1/ϕ

main = do
    print $ map fibonacci [0..10]

data Q = Q Rational Rational deriving Eq

instance Show Q where
    show (Q a b) = show a ++ (if b == 0 then "" else "+" ++ show b ++ "ϕ")

instance Num Q where
    fromInteger a = Q (fromInteger a) 0
    Q a b + Q a' b' = Q (a+a') (b+b')
    Q a b * Q a' b' = Q (a*a'+b*b') (a*b'+a'*b+b*b')
    negate (Q a b) = Q (negate a) (negate b)

instance Fractional Q where
    recip (Q a b) =
        let denominator = a*a+a*b-b*b
        in Q ((a+b)/denominator) (-b/denominator)

ϕ = Q 0 1
α = ϕ
β = 1-α

-- a = ϕ
-- b = 1-ϕ
-- √5 = 2ϕ-1
-- 1/√5 = (2ϕ-1)/5

sqrt5 = 2*ϕ-1

-- Term a b d = a(-1)ᵇⁿϕᶜⁿ
type Term b c = (b, c) -- Term a b c
type Term' = Term Bool Int

xor :: Bool -> Bool -> Bool
a `xor` b = a /= b

instance Monoid Bool where
    mempty = False
    mappend = xor

instance Group Bool where
    invert = id

instance Abelian Bool

instance Monoid Int where
    mempty = 0
    mappend = (+)

instance Group Int where
    invert = negate

instance Abelian Int

--instance (Show a, Show b, Show c) => Show (Term b c) where
--    show (b, c) = "(" ++ show a ++ ")*(-1)^" ++ show b ++"n*ϕ^(" ++ show c ++ "n)"

--instance (Num a, Monoid b, Monoid c) => Monoid (Term a b c) where
--    Term a b c `mappend` Term a' b' c' = Term (a*a') (b <> b') (c <> c')

--tPower :: (Fractional a, Num a, Abelian b, Abelian c) => Term a b c -> Int -> Term a b c
--tPower (Term a b c) n = Term (a^^n) (b `pow` n) (c `pow` n)

type Expr a b c = GroupRing a (b, c)
type Expr' = Expr Q Bool Int

{-
i :: (Monoid b, Monoid c) => a -> Expr a b c
i a = E [Term a mempty mempty]
-}

i :: Monoid g => r -> GroupRing r g
i r = GR [(mempty, r)]

j :: Num r => g -> GroupRing r g
j g = GR [(g, 1)]

newtype GroupRing r g = GR [(g, r)] deriving Show

reduce' :: (Num r, Eq r, Ord g) => [(g, r)] -> [(g, r)]
reduce' ts = filter (\(g, r) -> r /= 0) $ M.toList $ M.fromListWith (+) $ ts

instance (Num r, Eq r, Group g, Ord g) => Num (GroupRing r g) where
    fromInteger a = GR $ reduce' $ [(mempty, fromInteger a)]
    GR as+GR bs = GR $ reduce' $ as ++ bs
    GR as*GR bs = GR $ reduce' $ m <$> as <*> bs
                    where m (g, r) (g', r') = (g <> g', r*r')
    negate (GR as) = GR [(g, -r) | (g, r) <- as]

{-
instance (Eq a, Eq b, Eq c, Ord b, Ord c, Num a, Abelian b, Abelian c) => Num (Expr a b c) where
    fromInteger a = reduce $ i (fromInteger a)
    E as+E bs = reduce $ E (as ++ bs)
    E as*E bs = reduce $ E $ (<>) <$> as <*> bs
    negate (E as) = E [Term (-a) b c | Term a b c <- as]
    -}

{-
reduce :: (Eq a, Eq b, Eq c, Num a, Ord b, Ord c) => Expr a b c -> Expr a b c
reduce (E ts) = E $ map (\((a, b), c) -> Term c a b) $ filter (\((a, b), c) -> c /= 0) $ M.toList $ M.fromListWith (+) $ map (\(Term a b c) -> ((b, c), a)) ts
-}

ϕn, αn, βn, nn :: (Bool, Int)
ϕn = (False, 1)
αn = (False, 1)
βn = (True, -1)
nn = (True, 0)

--ePower :: (Fractional a, Num a, Abelian b, Abelian c) => Expr a b c -> Int -> Expr a b c
--ePower (E [a]) n = E [a `pow` n]

-- n Lucas(n)
-- 0 2
-- 1 1
-- 1 3
-- 2 4
-- 3 7
-- 5 11

evalTerm :: Int -> (Term Bool Int, Q) -> Q
evalTerm n ((b, c), r) = (-1)^^(if b then n else 0)*ϕ^^(c*n)*r

evalExpr :: Int -> Expr' -> Q
evalExpr n (GR ts) = sum (map (evalTerm n) ts)

--fib n = (ϕ^^n-(-1)^^n*ϕ^^(-n))/(2*ϕ-1)
--fib = E [Term (sqrt5/5) 0 1, Term (-sqrt5/5) 1 (-1)]
fib = (j ϕn-j βn)*i (1/sqrt5)

-- fib (an+b)
fib' a b = (i (α^^b)*(j $ ϕn `pow` a)-i (β^^b)*(j $ βn `pow` a))*i (1/sqrt5)
lucas' a b = (i (α^^b)*(j $ ϕn `pow` a)+i (β^^b)*(j $ βn `pow` a))

f 0 = 0
f 1 = 1
f n = f (n-1)+f (n-2)

ex1 :: GroupRing Q (Bool, Int)
ex1 = fib' 1 (-1)^2*fib' 1 1^2-fib' 1 (-2)^2*fib' 1 2^2-4*j nn*fib' 1 0^2

vmappend :: Monoid a => [a] -> [a] -> [a]
vmappend (a:as) (b:bs) = (a <> b : vmappend as bs)
vmappend a [] = a
vmappend [] b = b

trim as | all (== mempty) as = []
trim (a:as) = a : trim as

newtype V a = V [a] deriving (Eq, Show, Ord)

instance (Eq a, Monoid a) => Monoid (V a) where
    mempty = V []
    mappend (V a) (V b) = V $ trim (a `vmappend` b)

instance (Eq a, Group a) => Group (V a) where
    invert (V as) = V $ map invert as

instance (Eq a, Abelian a) => Abelian (V a) 

component :: Monoid a => V a -> Int -> a
component (V []) i = mempty
component (V (a : _)) 0 = a
component (V (a : as)) i = component (V as) (i-1)

ϕi, αi, βi, ni :: Expr Q (V Bool) (V Int)
ϕi = GR [((V [False], V [1]), 1)]
αi = GR [((V [False], V [1]), 1)]
βi = GR [((V [True], V [-1]), 1)]
ni = GR [((V [True], V []), 1)]

ϕj, αj, βj, nj :: Expr Q (V Bool) (V Int)
ϕj = GR [((V [False, False], V [0, 1]), 1)]
αj = GR [((V [False, False], V [0, 1]), 1)]
βj = GR [((V [False, True], V [0, -1]), 1)]
nj = GR [((V [False, True], V []), 1)]

ePower :: (Group g, Fractional r) => GroupRing r g -> Int -> GroupRing r g
ePower (GR [(g, r)]) n = GR [(g `pow` n, r ^^ n)]
-- fib'' a b c = fib_(a+b*m+c*n)
fib'' a b c = (i (α^^c)*(αi `ePower` a)*(αj `ePower` b)-i (β^^c)*(βi `ePower` a)*(βj `ePower` b))*i (1/sqrt5)
lucas'' a b c = (i (α^^c)*(αi `ePower` a)*(αj `ePower` b)+i (β^^c)*(βi `ePower` a)*(βj `ePower` b))

evalTerm' :: Int -> Int -> ((V Bool, V Int), Q) -> Q
evalTerm' i j ((b, c), a) = a*(-1)^^((if b `component` 0 then i else 0)+(if b `component` 1 then j else 0))*ϕ^^((c `component` 0)*i+(c `component` 1)*j)

evalExpr' :: Int -> Int -> Expr Q (V Bool) (V Int) -> Q
evalExpr' i j (GR ts) = sum (map (evalTerm' i j) ts)

{-
--
-- ∑ (-1)ᵇⁿϕᶜⁿ
-- = ∑ ((-1)ᵇϕᶜ)ⁿ
-- = (((-1)ᵇϕᶜ)ⁿ⁺¹-1) / ((-1)ᵇϕᶜ-1)
-- = 1/((-1)ᵇϕᶜ-1) * ((-1)ᵇⁿϕᶜⁿ(-1)ᵇϕᶜ-1)
sigma' :: Term Q Bool Int -> [Term Q Bool Int]
sigma' (Term a b c) =
    let denominator = if b then (-ϕ^^c-1) else ϕ^^c-1
    in [Term (a*(if b then -1 else 1)*ϕ^^c/denominator) b c, Term (-a/denominator) False 0]

sigma :: Expr' -> Expr'
sigma (E ts) = reduce $ E $ concatMap sigma' ts
-}

ex2 = fib'' 1 1 0+nj*fib'' 1 (-1) 0 - fib'' 1 0 0*lucas'' 0 1 0
ex3 = 2*fib'' 1 1 0-lucas'' 1 0 0*fib'' 0 1 0-lucas'' 0 1 0*fib'' 1 0 0
ex4 = fib'' 0 1 0^2*fib'' 1 0 1*fib'' 1 0 (-1)-fib'' 1 0 0^2*fib'' 0 1 1*fib'' 0 1 (-1)+nj*fib'' 1 1 0*fib'' 1 (-1) 0

{-
data Expression = Constant Int
                | Expression :+ Expression
                | Expression :* Expression
                | Expression :- Expression
                | Fib IExpression
                | Var Int deriving Show

instance Num Expression where
    (+) = (:+)
    (*) = (:*)
    (-) = (:-)
    fromInteger = Constant . fromInteger

m = Var 0
n = Var 1

translate (a:+b) = translate a + translate b
translate (a:-b) = translate a - translate b
translate (a:*b) = translate a * translate b
translate (Fib a) = ftranslate a

ftranslate
-}
