module QGates where

import Data.Complex
import QMatrix

data QGate = QGate (QMatrix) deriving (Show, Eq)

qGateMatrix :: QGate -> QMatrix
qGateMatrix (QGate mat) = mat

qGate :: QMatrix -> QGate
qGate mat = QGate mat

qGateDim :: QGate -> Int
qGateDim (QGate mat) = qDim mat

qGateShow (QGate qmat) = 
	do {
		putStrLn ("Gate dim = " ++ (show $ qGateDim (QGate qmat)));
		qShow qmat;
	}

hadamard2 = qGate ((1 / sqrt 2) `times` QMatrix [
			[1,  1],
			[1, -1]
		])
