{-
    author : afmika
    Bell Pair creation
-}

module QBellPair where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

run_bell_pair =
    let
        q0 = qBitFromVal 1 0 -- |0>
        q1 = qBitFromVal 1 1 -- |1>
        state0 = (hadamard2 `apply` q0) `combine` q1 -- H|0> (x) |1>
        state1 = cnot_cx `apply` state0 -- CNOT {H|0> (x) |1>}
        state2 = (hadamard2 `tprod` hadamard2) `apply` state1 -- (H(x)H) {CNOT {H|0> (x) |1>}}
        measured = qObserve $ state2 
    in
        do
            putStrLn $ "\nQBellPair"
            putStrLn $ "Theorical probabilities :"
            putStrLn $ "|R> = " ++ show state2
            putStrLn $ "After measuring"
            putStrLn $ qBitGraph measured 
