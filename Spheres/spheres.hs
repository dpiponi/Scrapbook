{-# LANGUAGE Strict #-}

import System.Random
import System.Random.Shuffle
import Linear.Vector
import Linear.V3
import Linear.Quaternion
import Control.Monad
import Data.Random.Normal
import Linear.Conjugate
import Linear.Metric

randQ :: IO (Quaternion Double)
randQ = do
    t <- normalIO
    x <- normalIO
    y <- normalIO
    z <- normalIO
    let q = Quaternion t (V3 x y z)
    return $ q/abs q

randQ' :: Double -> IO (Quaternion Double)
randQ' t = do
    x <- normalIO
    y <- normalIO
    z <- normalIO
    let xyz = normalize (V3 x y z) :: V3 Double 
    return $ Quaternion t (sqrt (1-t*t)*^xyz)

randQ'' :: IO (Quaternion Double)
randQ'' = do
    t <- normalIO
    x <- normalIO
    y <- normalIO
    z <- normalIO
    let q = Quaternion t (V3 x y z)
    return q

main2 = do
    let n = 1000000
    s <- replicateM n $ do
        q@(Quaternion t _) <- randQ
        r <- randQ' t
        return (q*r*q*r)
    print $ sum s/fromIntegral n

data Term = U | V deriving Show

--gen :: Int -> Int -> [Term]
--gen n m = (:) <$> [U, V]

interpret :: [Term] -> Quaternion Double -> Quaternion Double -> Quaternion Double
interpret [] u v = 1
interpret (U : ts) u v = u*interpret ts u v
interpret (V : ts) u v = v*interpret ts u v

main = do
    let n = 1000000
    s <- replicateM n $ do
        q <- randQ
        r <- randQ
        --let u = q*r*conjugate q*conjugate r
        --return $ (q+conjugate q)*(q+conjugate q)*(r+conjugate r)*(r+conjugate r)
        --return $ (q+conjugate q)*(r+conjugate r)*(q+conjugate q)*(r+conjugate r)
        let u = (q+conjugate q)
        let v = (r+conjugate r)
        let u2 = u*u-1
        let v2 = v*v-1
        return $ (u*u-1)*(v*v-1)*(u*u-1)*(v*v-1)
    putStrLn $ show (sum s/fromIntegral n)

