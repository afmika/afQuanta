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
	=> f is balanced if f(0) (+) f(1) = 1 otherwise f is constant
-}

a `xor` b = if a == b then 0 else 1
-- f x = 0 -- constant
-- f x = 1 -- constant
-- f x = x `xor` 1 -- balanced
f x = x `xor` 1

oracle_uf state = 
	let
		q0   = state !! 0
		q1   = state !! 1
		obq1 = qObserve q1
		tmp  = (qProbabilities $ obq1) !! 0
		-- 1*|0> + 0|1> or 0|0> + 1*|1> ??
		x    = if (areEqual tmp 1.0) then 0 else 1

		-- |x (+) f(x)>
		xr   = x `xor` (f x)
		comp = if xr == 1 then [0, 1] else [1, 0]
		q1'  = qBit $ qVec comp
	in
		[q0, q1']

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

		q0 = state2 !! 0
		q1 = state2 !! 1
	in
		do
			putStrLn $ "Deutsh algorithm -- is f balanced ?"
			putStrLn $ "Theorical probabilities :"
			putStrLn $ "q0 = " ++ (show $ qProbabilities q0)
			-- if f is balanced then q1 = |1> (since f 0 (+) f 1 = 1)
			-- otherwise q1 = 50%|0> or 50%|1> (our oracle_uf )
			putStrLn $ "q1 = " ++ (show $ qProbabilities q1)