----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QGate where

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

-- tensor product
tprod :: QGate -> QGate -> QGate
(QGate a) `tprod` (QGate b) = qGate $ a `kprod` b

qGateShow (QGate qmat) = 
	do {
		putStrLn ("Gate dim = " ++ (show $ qGateDim (QGate qmat)));
		qShow qmat;
	}

-- Gate composition
gdot :: QGate -> QGate -> QGate
(QGate a) `gdot` (QGate b) = qGate $ a `dot` b

-- Gate defs
-- Hadamard gate operates on a single qBit
hadamard2 = qGate ((1 / sqrt 2) `times` qMat [
			[1,  1],
			[1, -1]
		])

-- Pauli-X (= NOT gate) operates on a single qBit
pauli_x = qGate $ qMat [
			[0,  1],
			[1,  0]
		]

-- Pauli-Y operates on a single qBit
pauli_y = qGate $ qMat [
			[0,  (0 :+ 1)],
			[(0 :+ 1),  0]
		]
-- Pauli-Z operates on a single qBit
pauli_z = qGate $ qMat [
			[1,  0],
			[0,  -1]
		]

-- Swap (S) operates on 2 qBits
swap_s = qGate $ qMat [
			[1, 0, 0, 0],
			[0, 0, 1, 0],
			[0, 1, 0, 0],
			[0, 0, 0, 1]
		]

-- Toffoli (CCNOT) operates on 3 qBits
toffoli = qGate $ qMat [
			[1, 0, 0, 0, 0, 0, 0, 0],
			[0, 1, 0, 0, 0, 0, 0, 0],
			[0, 0, 1, 0, 0, 0, 0, 0],
			[0, 0, 0, 1, 0, 0, 0, 0],
			[0, 0, 0, 0, 1, 0, 0, 0],
			[0, 0, 0, 0, 0, 1, 0, 0],
			[0, 0, 0, 0, 0, 0, 0, 1],
			[0, 0, 0, 0, 0, 0, 1, 0]
		]