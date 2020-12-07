module QTestCases where

import Data.Complex
import Data.List
import QVector
import QMatrix
import QBit
import QGate


-- test Definition
expect info a b
	| a /= b    = putStrLn $ info ++ " ------- [Fail]"
	| otherwise = putStrLn $ info ++ " ------- [ ok ]"



run_tests = 
	let 
		u = qVec [0 :+ 1, 0]
		v = qVec [1, 0]
		w = qVec $ take 4 (repeat 0.5)	
		z = qVec [1, 1.5]
	in do
		putStrLn "\n----------- QTestCases ----------"

		expect "Assertion to be True           " True True
		expect "areEqual 0 1/10^-7 to be True " (areEqual 0 0.0000001 ) True
		expect "areEqual 0 1/10^-6 to be False" (areEqual 0 0.00001 ) False
		expect "sqrt Re(<u|u>) = qVLength u to be True" (areEqual (sqrt $ realPart $ u `vdot` u) (qVLength $ u)) True
		expect "qVNormalize u to be unitary" (areEqual 1 (qVLength $ qVNormalize u)) True
		expect "qVNormalize v to be unitary" (areEqual 1 (qVLength $ qVNormalize v)) True
		expect "qVNormalize w to be unitary" (areEqual 1 (qVLength $ qVNormalize w)) True