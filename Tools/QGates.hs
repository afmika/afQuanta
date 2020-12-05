module QGates where

import Data.Complex
import QVector
import QMatrix
import QBit

data QGate = QGate (QMatrix) deriving (Show, Eq)

qGateMatrix :: QGate -> QMatrix
qGateMatrix (QGate mat) = mat

qGate :: QMatrix -> QGate
qGate mat = QGate mat

qGateDim :: QGate -> Int
qGateDim (QGate mat) = qDim mat

apply :: QGate -> QBit -> QBit
gate `apply` bit =
	let
		mat = qGateMatrix gate
		vec = qBitVec bit
	in
		qBit (mat `mtimes` vec)

qGateShow (QGate qmat) = 
	do {
		putStrLn ("Gate dim = " ++ (show $ qGateDim (QGate qmat)));
		qShow qmat;
	}

hadamard2 = qGate ((1 / sqrt 2) `times` qMat [
			[1,  1],
			[1, -1]
		])
