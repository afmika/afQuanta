module QVector where
import Data.Complex

data QVector = QVector [Complex Float] deriving (Show, Eq)

qVec :: [Complex Float] -> QVector
qVec list = QVector list

qVDim :: QVector -> Int
qVDim (QVector u) = length u

qVMap :: (Complex Float -> Complex Float) -> QVector -> QVector
qVMap func (QVector xs) = QVector $ map func xs;

vplus :: QVector -> QVector -> QVector
(QVector u) `vplus` (QVector v) 
	| length u /= length v = error "operands must have the same dimension"
	| otherwise  = QVector [ (u !! i) + (v !! i) | i <- [0 .. length u - 1]]

vdot :: QVector -> QVector -> (Complex Float)
(QVector u) `vdot` (QVector v) = sum [ (u !! i) * (v !! i) | i <- [0 .. length u - 1] ]