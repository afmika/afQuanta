module Main where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGates

main :: IO ()
main = 
	let
		state  = qVec [1 :+ 0, 2, 1 :+ 1, 1]
		transf = qEye (qVDim state)
		gate   = qGate transf
		comp   = map fromIntegral $ take 4 [1..]
		psi    = qBit $ qVNormalize $ qVec $ comp
		prod   = qMat [[1, 2], [3, 4]] `dot` qMat [[1, 2], [1, -1]] `dot` qEye 2
 	in
 		do {
 			putStrLn("----------");
	 		putStrLn (show psi);
	 		putStrLn ("N = " ++ (show $ qBitDim psi));
	 		qGateShow hadamard2;
	 		qShow prod;
	 	}