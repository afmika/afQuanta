module Main where

import Data.Complex
import QVector
import QMatrix
import QBit

main :: IO ()

main = 
	let 
		state  = qVec [1, 2, 1 :+ 1, 1]
		transf = qEye (qVDim state)
		psi    = qBit $ QVector [1 / sqrt 2, 1 / sqrt 2]
 	in
 		do {
	 		putStrLn (show psi);
		 	qShow transf
	 	}
