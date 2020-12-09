----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QVector where

import QMath
import Data.Complex
import Data.List

-- helpers
areEqual :: Double -> Double -> Bool
areEqual a b = abs(a - b) <= v_upsilon

-- defs
data QVector = QVector [Complex Double] deriving (Eq)
qVec :: [Complex Double] -> QVector
qVec list = QVector list

qVAsList :: QVector -> [Complex Double]
qVAsList (QVector list) = list

-- usual ops
vplus :: QVector -> QVector -> QVector
(QVector u) `vplus` (QVector v) 
	| length u /= length v = error "operands must have the same dimension"
	| otherwise  = qVec [ (u !! i) + (v !! i) | i <- [0 .. length u - 1]]

-- vector size
qVDim :: QVector -> Int
qVDim (QVector u) = length u

-- inner product <u|v> = u1* v1 + u2* v2 + ... where ui* = (x + iy)* = (x - iy)
vdot :: QVector -> QVector -> Complex Double
(QVector u) `vdot` (QVector v)
	| length u /= length v = error "operands must have the same dimension"
	| otherwise            = 
		sum [ conjugate (u !! i) * (v !! i) | i <- [0 .. length u - 1] ]

vtimes :: Double -> QVector -> QVector
n `vtimes` (QVector v) = qVec [ (n :+ 0) * (v !! i) | i <- [0 .. length v - 1] ]

qVMap :: (Complex Double -> Complex Double) -> QVector -> QVector
qVMap func (QVector xs) = qVec $ map func xs;

-- magnitude u = sqrt <u|u> or sqrt Sum |ui|^2  (since ui* ui = |ui|^2 )
qVLength :: QVector -> Double
qVLength u = sqrt $ realPart $ u `vdot` u

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

instance Show QVector where
	show (QVector xs) = "[" ++ intercalate ", " (map formatComplexStrict xs) ++ "]"