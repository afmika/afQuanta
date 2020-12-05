----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QBit where

import QVector
import QRandom

import Data.Complex
import System.IO.Unsafe ( unsafePerformIO )

data QBit = QBit (QVector) deriving (Show, Eq)

qBit :: QVector -> QBit
qBit vec
	| not (qHasDimPowOf2 vec) || ((qVDim vec) == 1) = error "Vec dimension should be a power of 2 and /= 1" 
	| not (qIsVUnitary vec)   = error "Vec should be unitary"
	| otherwise = QBit vec

qBitVec :: QBit -> QVector
qBitVec (QBit v) = v

qBitDim :: QBit -> Int
qBitDim qb = qVDim $ qBitVec qb


qProbabilities :: QBit -> [Float]
qProbabilities (QBit (QVector xs)) = [ realPart ((abs m)^2) | m <- xs]


-- IO Operations (actions)
qRandPickIndex :: [Float] -> Int -> Bool -> IO Int
qRandPickIndex xs i True = do return i
qRandPickIndex xs i done = do
	rand  <- qIORandFloat
	if rand <= (xs !! i) then 
		qRandPickIndex xs i True
	else 
		qRandPickIndex xs ((i + 1) `mod` (length xs)) False

qIOObserve :: QBit -> IO QBit
qIOObserve (QBit (QVector xs)) = do
	picked <- qRandPickIndex (qProbabilities $ QBit (QVector xs)) 0 False
	let len   = length xs
	let index = [0 .. len - 1]
	let pickMe = \i -> if i /= picked then (0 :+ 0) else (1 :+ 0)
	return $ qBit $ qVec [ pickMe i | i <- index ]


-- IO QBit --> QBit
qObserve qb = unsafePerformIO $ qIOObserve qb