----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QRandom where

import Data.Time
import Data.Ratio
import System.IO.Unsafe ( unsafePerformIO )

-- helpers

bigint = 2147483562

-- random generator

-- Xnext = (aXn + c) mod m
-- For a = 16807, c = 0, m = 2^31 - 1
-- has a period of 2^31 - 2
qPRNGDef :: Int -> Int
qPRNGDef x = (16807 * x `mod` bigint)

qRand :: Int -> Int -> Int
qRand seed 0   = qPRNGDef seed
qRand seed iteration = qRand (qPRNGDef seed) (iteration - 1)


-- IO Operation (by which the outside world communicates with the program)
qCurrentTime :: IO Double
qCurrentTime = do
        utc <- getCurrentTime
        let 
            dtime = fromRational $ toRational $ utctDayTime utc
        return dtime

qIORand :: IO (Int)
qIORand = do
    dtime <- qCurrentTime
    let 
        seed = fromInteger $ floor (10000 * dtime)
    return $ qRand seed 50


qIORandInf :: Int -> IO (Int)
qIORandInf limit = do
    out <- qIORand
    return (out `mod` limit)

qIORandFloat :: IO (Double)
qIORandFloat = do
    out <- qIORandInf bigint
    return (fromIntegral out / fromIntegral bigint)

-- generates a random list
qIORandSequenceCmplx :: [Int] -> Bool -> Int -> Int -> IO [Int]
qIORandSequenceCmplx xs begin current_seed limit
    | limit == 0   = return xs
    | otherwise    = do
        let next_seed = qRand current_seed 1
        return $ unsafePerformIO $ qIORandSequenceCmplx (xs ++ [next_seed]) False next_seed (limit - 1)

qIORandSequence :: Int -> IO [Int]
qIORandSequence limit = do
    dtime <- qCurrentTime
    let seed = fromInteger $ floor (10000 * dtime)
    return $ unsafePerformIO $ qIORandSequenceCmplx [] True  (fromInteger seed) limit

qGetRandSeq limit = unsafePerformIO $ qIORandSequence limit


qRandPickIndex :: [Double] -> Int -> Bool -> IO Int
qRandPickIndex xs i True = do return i
qRandPickIndex xs i done = do
    rand  <- qIORandFloat
    if rand <= (xs !! i) then 
        qRandPickIndex xs i True
    else 
        qRandPickIndex xs ((i + 1) `mod` (length xs)) False

qRandPickIndexUniform :: [Double] -> IO Int
qRandPickIndexUniform xs = do qIORandInf (length xs)