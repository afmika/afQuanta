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

-- checks if all probabilities are equal (the index should be uniformly picked)
qEqualProbabilities :: QBit -> Bool
qEqualProbabilities qb = 
	let
		probs = qProbabilities qb
		first = probs !! 0
		func  = \a b -> if areEqual a b then 1 else 0 
	in
		(length probs) == (sum [ func first p | p <- probs])

-- IO Operations (actions)
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

qIOObserve :: QBit -> IO QBit
qIOObserve (QBit (QVector xs)) = do
	picked <- 
			if (qEqualProbabilities $ QBit (QVector xs)) then 
				qRandPickIndex (qProbabilities $ QBit (QVector xs)) 0 False
			else
				qRandPickIndexUniform (qProbabilities $ QBit (QVector xs))

	let len   = length xs
	let index = [0 .. len - 1]
	let pickMe = \i -> if i /= picked then (0 :+ 0) else (1 :+ 0)
	return $ qBit $ qVec [ pickMe i | i <- index ]


-- IO QBit --> QBit
qObserve qb = unsafePerformIO $ qIOObserve qb