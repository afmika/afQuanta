{-
	author : afmika
	A simple algorithm to perform an addition 
-}

module QAdder where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

run_adder_algorithm =
	let
		cnot3 = cnot_cx `gdot` cnot_cx `gdot` cnot_cx
		q0  = qBit $ qVec [1, 0, 1, 0, 0, 0, 1, 0] -- |1 0 1 0 1 0>
		output = q0
		measured = qObserve q0 
	in
		do
			putStrLn $ "\nQAdder"
			putStrLn $ "Theorical probabilities :"
			putStrLn $ "|R> = " ++ show output
			putStrLn $ qBitGraph output
			putStrLn $ "After measuring"
			putStrLn $ qBitGraph measured 
