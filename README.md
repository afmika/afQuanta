# afQuanta
afQuanta is a basic quantum computing library written in Haskell

# Basic use
```Haskell
module Main where
import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

main :: IO ()
main = 
	let
		-- Initializing the register state
		-- |psi> = 1*|0> + 0*|1>
		psi = qBit $ qVec [1, 0]

		-- Applying the Hadamard gate
		-- H |psi> = 1/sqrt 2  (|0> + |1>)
		result   = hadamard2 `apply` psi
		measured = qObserve result 
	in
		do
			putStrLn ("|psi> = " ++ show psi)
			putStrLn ("H |psi> = " ++ show result)
			putStrLn ("mes H|psi> : \n" ++ show measured)
```