module QBit where

import QVector
import Data.Complex

data QBit = QBit (QVector) deriving (Show, Eq)

qBit :: QVector -> QBit
qBit vec
	| not (isUnitary vec) = error "Vec should be unitary"
	| otherwise = QBit vec

qBitVec :: QBit -> QVector
qBitVec (QBit v) = v

qBitDim :: QBit -> Int
qBitDim qb = qVDim $ qBitVec qb