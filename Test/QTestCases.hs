module QTestCases where

import Data.Complex
import Data.List
import QVector
import QMatrix
import QBit
import QGate
import QRandom
import System.IO.Unsafe ( unsafePerformIO )



-- test Definition
expect info a b
	| a /= b    = putStrLn $ " [Fail] -- Expect " ++ info
	| otherwise = putStrLn $ " [ ok ] -- Expect " ++ info

random_test rand xs 0     = return xs
random_test rand xs count = do
	let nxs = xs ++ [if (unsafePerformIO rand) < 5 then 1 else 0]
	random_test (qIORandInf 10) nxs (count - 1)


run_tests = 
	let 
		u = qVec [0 :+ 1, 0]
		v = qVec [1, 0]
		w = qVec $ take 4 (repeat 7)
		z = qVec [1, 1.5]
	in do
		putStrLn "\n-- QTestCases --"

		expect "Assertion to be True" True True
		expect "areEqual 0 1/10^-7 to be True " (areEqual 0 0.0000001 ) True
		expect "areEqual 0 1/10^-6 to be False" (areEqual 0 0.00001 ) False
		expect "sqrt Re(<u|u>) = qVLength u to be True" (areEqual (sqrt $ realPart $ u `vdot` u) (qVLength $ u)) True
		
		expect "qVNormalize u to be unitary" (areEqual 1 (qVLength $ qVNormalize u)) True
		expect "qVNormalize v to be unitary" (areEqual 1 (qVLength $ qVNormalize v)) True
		expect "qVNormalize w to be unitary" (areEqual 1 (qVLength $ qVNormalize w)) True

		expect "qHasDimPowOf2 u to be True" (qHasDimPowOf2 u) True
		expect "qHasDimPowOf2 $ qVec [1,1,1] to be False" (qHasDimPowOf2 $ qVec [1,1,1]) False

		expect "qIsZero $ qMat [[0, 0], [0.001, 0]] to be False"
			(qIsZero $ qMat [[0, 0], [0.001, 0]]) False
		expect "qIsZero $ qMat [[0, 0], [0, 0]] to be True"
			(qIsZero $ qMat [[0, 0], [0, 0]]) True

		expect "qIsUnitary H (x) toffoli to be True" 
			(qIsUnitary $ qGateMatrix $ (hadamard2 `tprod` toffoli)) True
		expect "qIsUnitary (qGateGenerator 1 2 3) to be True" 
			(qIsUnitary $ qGateMatrix $ qGateGenerator 1 2 3) True
		
		expect "qIsHermitian pauli_z to be True"
			(qIsHermitian $ qGateMatrix pauli_z) True
		expect "qIsHermitian hadamard2 to be True"
			(qIsHermitian $ qGateMatrix hadamard2) True
		expect "qIsHermitian $ qMat [[1, 2], [3, 4]] to be False"
			(qIsHermitian $ qMat [[1, 2], [3, 4]]) False

		expect "(H (x) H) |u>(x)|v> to be have equal probabilities" 
			(foldl (&&) 
				True (zipWith areEqual 
						(qProbabilities $ (hadamard2 `tprod` hadamard2) `apply` (qBit u `combine` qBit v))
						[0.25, 0.25, 0.25, 0.25]
					)) 
			True
		-- print $ unsafePerformIO (random_test (qIORandInf 10) [] 50)