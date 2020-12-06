----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QVector where

import QMath
import Data.Complex

-- helpers
areEqual :: Double -> Double -> Bool
areEqual a b = abs(a - b) <= v_upsilon

-- defs
data QVector = QVector [Complex Double] deriving (Show, Eq)
qVec :: [Complex Double] -> QVector
qVec list = QVector list

qVAsList :: QVector -> [Complex Double]
qVAsList (QVector list) = list

-- usual ops
vplus :: QVector -> QVector -> QVector
(QVector u) `vplus` (QVector v) 
	| length u /= length v = error "operands must have the same dimension"
	| otherwise  = qVec [ (u !! i) + (v !! i) | i <- [0 .. length u - 1]]

vdot :: QVector -> QVector -> (Complex Double)
(QVector u) `vdot` (QVector v) = sum [ (u !! i) * (v !! i) | i <- [0 .. length u - 1] ]

vtimes :: Double -> QVector -> QVector
n `vtimes` (QVector v) = qVec [ (n :+ 0) * (v !! i) | i <- [0 .. length v - 1] ]

-- others
qVDim :: QVector -> Int
qVDim (QVector u) = length u

qVMap :: (Complex Double -> Complex Double) -> QVector -> QVector
qVMap func (QVector xs) = qVec $ map func xs;

qVLength :: QVector -> Double
qVLength (QVector xs) = sqrt $ realPart $ sum $ map (\a -> (abs a) ^ 2) xs

qIsVUnitary :: QVector -> Bool
qIsVUnitary vec = areEqual 1.0 (qVLength vec)

qVNormalize :: QVector -> QVector
qVNormalize vec =
	let
		len  = qVLength vec
	in 
		(1 / len) `vtimes` vec

qHasDimPowOf2 :: QVector -> Bool
qHasDimPowOf2 vec = 
	let
		dim    = fromIntegral (qVDim vec)
		n      = log dim / log 2
		floorn = fromIntegral (floor n)
	in
		areEqual n floorn