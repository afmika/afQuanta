{-
    author : afmika
    Grover's algorithm using the amplitude amplification trick
    This example uses 2-qubits (can operate on 2^2 = 4 items)
-}

module QGrover where

import Data.Complex
import QVector
import QMatrix
import QBit
import QGate

-- oracle matrix
uw = qGateCreate [
        [1, 0, 0, 0],-- |00> --
        [0, 1, 0, 0],-- |01> --
        [0, 0, 1, 0],-- |10> --
        [0, 0, 0,-1] -- |11> <= our guy
    ]

run_grover_algorithm =
    let
        -- initializing
        reg_size = 1
        q0  = qBitFromVal reg_size 0 -- |0>
        q1  = qBitFromVal reg_size 0 -- |0>
        reg = q0 `combine` q1 -- |00>

        -- H on the input
        reg' = (hadamard2 `tprod` hadamard2) `apply` reg
        -- Applying the oracle
        or_out = uw `apply` reg'

        -- diffusor
        h_out   = (hadamard2 `tprod` hadamard2) `apply` or_out
        z_out   = (pauli_z `tprod` pauli_z) `apply` h_out
        cz_gate = uw
        cz_out  = cz_gate `apply` z_out
        output  = (hadamard2 `tprod` hadamard2) `apply` cz_out
        ---------

        measured = qObserve output 
    in
        do
            putStrLn $ "\nGrover's algorithm"
            putStrLn $ "Theorical probabilities :"
            putStrLn $ "|R> = " ++ show output
            putStrLn $ qBitGraph output
            putStrLn $ "After measuring"
            putStrLn $ qBitGraph measured 
