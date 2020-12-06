----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module Main where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

main :: IO ()
main = 
	let
		state  = qVec [1 :+ 0, 2, 1 :+ 1, 1] -- constructs a new QVector
		transf = qEye (qVDim state) -- builds an Id matrix with the same dim
		gate   = qGate transf -- makes a new gate using the transf matrix
		
		-- builds a qBit from a normalized 2^N vector
		comp   = map fromInteger $ take 4 (repeat 1)
		psi    = qBit $ qVNormalize $ qVec comp
		observed_psi  = qObserve psi
		observed_prob = qProbabilities observed_psi

		-- examples
		prod   = qMat [[1, 2], [3, 4]] `dot` qMat [[1, 2], [1, -1]] `dot` qEye 2 -- dot between matrices
		result = (qMat [[1, 3], [-1, 1]]) `mtimes` (qVec [1, 4]) -- dot between a matrix and a vector

		-- H |0> ---> 1/sqrt 2 ( |0> + |1> )
		state0   = qBit $ qVec [1, 0]
		res_bit  = hadamard2 `apply` state0 -- dot between a Gate and a qBit
		probs    = qProbabilities res_bit
		measured = qObserve res_bit
		mes_prob = qProbabilities measured
 	in
 		do
 			putStrLn $ "-----------"
	 		putStrLn $ "psi = " ++ show psi
	 		putStrLn $ "Observed psi   = " ++ show observed_psi
	 		putStrLn $ "Observed probs = " ++ show observed_prob

	 		putStrLn $ "\nHadamard Gate"
	 		qGateShow hadamard2

	 		putStrLn $ "A.B"
	 		qShow prod

	 		putStrLn $ "Mat . Vec = " ++ show result ++ "\n"
	 		putStrLn $ "H |0> = " ++ show res_bit
	 		putStrLn $ "H |0> probs     = " ++ show probs
	 		putStrLn $ "Observed H |0>  = " ++ show measured
	 		putStrLn $ "Observed probs  = " ++ show mes_prob