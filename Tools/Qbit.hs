----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QBit where

import QVector
import Data.Complex
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

qObserve :: QBit -> QBit
qObserve (QBit (QVector xs)) = 
	let
		len   = length xs
		index = [0 .. len - 1] 
		ridx  = 0 -- @todo should be picked randomly according to the qBit components
		pick  = \i -> if i /= ridx then 0 else 1
	in
		qBit $ qVec [ pick i | i <- index ]