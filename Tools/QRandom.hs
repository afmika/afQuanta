----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QRandom where

import Data.Time
import Data.Ratio

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
qCurrentTime :: IO (Integer, Integer)
qCurrentTime = do
		utc <- getCurrentTime
		let 
			dtime = toRational $ utctDayTime utc
			sec   = numerator dtime
			psec  = denominator dtime
		return (sec, psec)

qIORand :: IO (Int)
qIORand = do
	(sec, psec) <- qCurrentTime
	let
		seed = sec
		iter = 1 + (psec `mod` 99)
	return $ qRand (fromInteger seed) (fromInteger iter)


qIORandInf :: Int -> IO (Int)
qIORandInf limit = do
	out <- qIORand
	return (out `mod` limit)

qIORandFloat :: IO (Float)
qIORandFloat = do
	out <- qIORandInf bigint
	return (fromIntegral out / fromIntegral bigint)


qRandPickIndex :: [Float] -> Int -> Bool -> IO Int
qRandPickIndex xs i True = do return i
qRandPickIndex xs i done = do
	rand  <- qIORandFloat
	if rand <= (xs !! i) then 
		qRandPickIndex xs i True
	else 
		qRandPickIndex xs ((i + 1) `mod` (length xs)) False

qRandPickIndexUniform :: [Float] -> IO Int
qRandPickIndexUniform xs = do qIORandInf (length xs)