----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QBit where

import QMath
import QVector
import QRandom

import Data.Complex
import Data.List
import System.IO.Unsafe ( unsafePerformIO )

data QBit = QBit (QVector) deriving (Show, Eq)

qBit :: QVector -> QBit
qBit vec
	| not (qHasDimPowOf2 vec) || ((qVDim vec) == 1) = error "Vec dimension should be a power of 2 and /= 1" 
	| not (qIsVUnitary vec)   = error "Vec should be unitary"
	| otherwise = QBit vec

qBitVec :: QBit -> QVector
qBitVec (QBit v) = v

qBitAsList :: QBit -> [Complex Double]
qBitAsList (QBit (QVector xs)) = xs

qBitDim :: QBit -> Int
qBitDim qb = qVDim $ qBitVec qb

-- inner product between two qubits
qdot :: QBit -> QBit -> Complex Double
(QBit a) `qdot` (QBit b) = a `vdot` b

qProbabilities :: QBit -> [Double]
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

-- tensor product between two qubits
-- We can use this operator to combine two qubits
-- Assuming the two operands are not entangled
combine :: QBit -> QBit -> QBit
(QBit (QVector a)) `combine` (QBit (QVector b)) = 
	let
		ta = transpose [a]
		tb = transpose [b]
		kr = kroneker_prod ta tb
	in
		qBit $ qVec $ map (\i -> i !! 0 ) kr



-- IO Operations (actions)

qIOObserve :: QBit -> IO QBit
qIOObserve (QBit (QVector xs)) = do
	picked <- 
			if (qEqualProbabilities $ QBit (QVector xs)) then 
				qRandPickIndexUniform (qProbabilities $ QBit (QVector xs))
			else
				qRandPickIndex (qProbabilities $ QBit (QVector xs)) 0 False

	let len   = length xs
	let index = [0 .. len - 1]
	let pickMe = \i -> if i /= picked then (0 :+ 0) else (1 :+ 0)
	return $ qBit $ qVec [ pickMe i | i <- index ]


-- IO QBit --> QBit
qObserve qb = unsafePerformIO $ qIOObserve qb


qBitInterpret :: QBit -> String
qBitInterpret qb =
	if qEqualProbabilities qb then "State = In superposition"
	else 
		let -- observed
			xs  = qProbabilities qb
			len = length xs
			f   = \i -> if areEqual (xs !! i) 1.0 then i else 0
			idx = sum [ f i | i <- [0 .. len - 1]]
		in
			"State = |" ++ (show idx) ++ ">"