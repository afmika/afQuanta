module QDeutsh where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

{-
	Given an unknown function f : {0, 1} -> {0, 1}
	Compute f(x) (+) f(y)

	=> Classical solution, two queries : a = f(x), b = f(y) then a (+) b 	
	=> Quantum solution, a single query is enough!
	=> f is balanced -> f(0) (+) f(1) = 1 otherwise f is constant
-}

a `xor` b = if a == b then 0 else 1
-- f x = 0 -- constant
-- f x = 1 -- constant
-- f x = x `xor` 1 -- balanced
f x = x `xor` 1

oracle_uf state = 
	let
		q0  = state !! 0
		q1  = state !! 1
		-- in the real world, this operation would take a single query
		q0' = 
			(qGate $ qMat [
					[(-1)^(f 0), 0], 
					[0, (-1)^(f 1)]
			]) `apply` q0 
	in
		[q0', q1]

run_deutsh_algorithm =
	let
		state0 = [
				qBit $ qVec [1, 0], -- |0>
				qBit $ qVec [1 / sqrt 2, -1 / sqrt 2] -- 1/ sqrt 2 (|1> - |0>)
			]

		-- Hadamard on the first register
		state1 = [
				hadamard2 `apply` (state0 !! 0),
				state0 !! 1
			]

		-- Uf on both registers
		state2 = oracle_uf state1 

		q0 = hadamard2 `apply` (state2 !! 0)
		q1 = state2 !! 1

		measured = qObserve q0
	in
		do
			putStrLn $ "Deutsh algorithm -- is f balanced ?"
			putStrLn $ "Theorical probabilities :"
			putStrLn $ "q0 = " ++ (show $ qProbabilities q0)
			putStrLn $ "q1 = " ++ (show $ qProbabilities q1)
			putStrLn $ "Result :"
			putStrLn $ qBitInterpret measured
