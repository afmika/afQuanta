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

random_test :: Int -> [Int]
random_test limit = 
	-- generates a list of odd numbers <= 7 : {1, 3, 5, 7}
	-- should get 50%, 50% for {1,3}, {5, 7} 
	map (\x -> if (x `mod` 8) < 5 then 1 else 0) $ qGetRandSeq limit

run_tests = 
	let 
		u = qVec [0 :+ 1, 0]
		v = qVec [1, 0]
		w = qVec $ take 4 (repeat 7)
		z = qVec [1, 1.5]

		rnd_sum = sum (random_test 10000)
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
		expect "rnd_sum to be somewhere between ]4500, 5500[ " (rnd_sum > 4500 && rnd_sum < 5500) True