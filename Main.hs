module Main where

import Data.Complex
import QVector
import QMatrix

main :: IO ()

main = 
	let 
		state  = qVec [1, 2, 1 :+ 1, 1]
		transf = qEye (qVDim state)
 	in
	 	qShow transf
