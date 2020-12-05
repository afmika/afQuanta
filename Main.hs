module Main where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGates

main :: IO ()
main = 
	let 
		state  = qVec [1, 2, 1 :+ 1, 1]
		transf = qEye (qVDim state)
		gate   = QGate transf
		comp   = map fromIntegral $ take 4 [1..]
		psi    = qBit $ qVNormalize $ qVec $ comp
 	in
 		do {
	 		putStrLn (show psi);
	 		putStrLn ("N = " ++ (show $ qBitDim psi));
	 		qGateShow hadamard2;
	 	}