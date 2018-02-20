{- LANGUAGE UnicodeSyntax -}

import Prelude hiding (sum)
import Control.Monad
import qualified System.Random as R
import qualified Data.Map.Strict as M
import Control.Monad.State

instance (Eq r, Num r) => Num [r] where
    fromInteger x = fromInteger x : repeat 0

    x+y      = zipWith (+) x y
    negate x = map negate x

    a       *(0 : bs) = 0 : a*bs
    (a : as)*(b : bs) = a*b : zipWith (+) (map (a *) bs) (as*(b : bs))

    signum (a : _) = signum a : repeat 0
    abs (a : as)   = abs a : map (signum a *) as

instance (Eq r, Fractional r) => Fractional [r] where
    fromRational x = fromRational x : repeat 0

    recip (x0 : xs) = r where r = map (/ x0) (zipWith (-) 1 (0 : r*xs))

d (_ : x) = zipWith (*) (map fromInteger [1..]) x

integrate a x = a : zipWith (/) x (map fromInteger [1..])

instance (Eq r, Floating r) => Floating [r] where
    sqrt (a : x) = map (sqrt a *) $ 1 : sqrt' (map (/ a) x)
                   where sqrt' xs = rs where rs = map (/ 2) (zipWith (-) xs (0 : rs*rs))

    exp x@(x0 : _)   = e where e = integrate (exp x0) (e*d x)
    log x@(x0 : _)   = integrate (log x0) $ d x/x

    sin x@(x0 : _)   = integrate (sin x0) $ cos x*d x
    cos x@(x0 : _)   = integrate (cos x0) $ -sin x*d x

    asin x@(x0: _)   = integrate (asin x0) $ d x/sqrt(1-x*x)
    acos x@(x0: _)   = integrate (acos x0) $ -d x/sqrt(1-x*x)
    atan x@(x0: _)   = integrate (atan x0) $ d x/(1+x*x)

    sinh x@(x0 : _)  = integrate (sinh x0) $ cosh x*d x
    cosh x@(x0 : _)  = integrate (cosh x0) $ sinh x*d x

    asinh x@(x0 : _) = integrate (asinh x0) $ d x/sqrt(1+x*x)
    acosh x@(x0 : _) = integrate (acosh x0) $ d x/sqrt(x*x-1)
    atanh x@(x0 : _) = integrate (atanh x0) $ d x/(1-x*x)

    pi               = pi : repeat 0

-- ε is an infinitesimal all of whose powers we track
ε :: Fractional a => [a]
ε = 0 : 1 : repeat 0

z = ε

derivs a = zipWith (*) a $ scanl (*) 1 [1..]

--
-- Free monad giving generic probability interface.
-- Interpreter below.
--
-- See http://blog.sigfpe.com/2017/06/a-relaxation-technique.html
--
data Random p a = Pure a | Bernoulli p (Int -> Random p a)

instance Functor (Random p) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Bernoulli p g) = Bernoulli p (fmap f . g)

instance Applicative (Random p) where
    pure = return
    (<*>) = ap

instance Monad (Random p) where
    return = Pure
    Pure a >>= f = f a
    Bernoulli p g >>= f = Bernoulli p (\x -> g x >>= f)

bernoulli :: p -> Random p Int
bernoulli p = Bernoulli p return

scale :: Num p => p -> [(a, p)] -> [(a, p)]
scale s = map (\(a, p) -> (a, s*p))

collect :: (Ord a, Num b) => [(a, b)] -> [(a, b)]
collect = M.toList . M.fromListWith (+)

--
-- Interpreter for our free monad.
-- This provides "weighted importance sampling" semantics with
-- a rule to specify how a weight is converted to a probability.
-- The interpreter generates random samples that also carry an
-- importance.
-- For a simple sampling based interpreter the rule can be the
-- identity.
--
interpret :: (Fractional p, R.RandomGen g) =>
              (p -> Float) -> Random p a -> State g (a, p)
interpret rule (Pure a) = return (a, 1)
interpret rule (Bernoulli p f) = do
    r <- state R.random
    let prob = rule p
    let (b, i) = if (r :: Float) <= prob
                      then (1, p/realToFrac prob)
                      else (0, (1-p)/realToFrac (1-prob))
    (a, i') <- interpret rule (f b)
    return (a, i*i')

--
-- Compute expected values taking into account weights.
--
expect :: (Fractional p, R.RandomGen g) =>
           (p -> Float) -> Random p p -> Int -> g -> (p, g)
expect rule r n g = 
    let (x, g') = runState (sum rule 0 r n ) g
    in (x/fromIntegral n, g')

sum :: (Fractional p, R.RandomGen g) =>
        (p -> Float) -> p -> Random p p -> Int -> State g p
sum rule t r 0 = return (t, g)
sum rule t r n = do
    (a, imp) <- interpret rule r)
    sum rule (t+a*imp) r (n-1)

-- Example from https://www.arxiv-vanity.com/papers/1802.05098/
-- See section 3.3 "Simple Failing Example"
--
-- We're able to correctly differentiate the expected value of
--
-- X(1-θ)+(1-X)(1+θ)
--
-- arbitrarily many times even though X is sampled from Ber(θ)
-- and so only ever takes integer values.
-- 
ex1 θ = do
    x <- bernoulli θ
    return $ fromIntegral x*(1-θ)+(1-fromIntegral x)*(1+θ)

--
-- Get just the value of a power series evaluated at zero killing
-- all of the derivatives.
--
(⊥) = head

main = do
    --
    -- Evaluate derivative from example at θ = 0.5
    --
    e <- R.getStdRandom (expect (⊥) (ex1 (0.5+ε)) 1000)
    print $ take 6 $ cos (acos (0.5+ε))
    print $ take 6 $ sin (asin (0.5+ε))
    print $ take 6 $ atanh (tanh (0.5+ε))
    print $ take 6 $ acosh (cosh (0.5+ε))
    print $ take 6 $ asinh (sinh (0.5+ε))
    print $ take 6 $ log (exp (0.5+ε))
    print $ take 6 $ exp (log (0.5+ε))
    print $ take 6 $ tan (atan (0.5+ε))
    print $ take 6 $ derivs $ sin ε
