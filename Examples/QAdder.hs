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
        ccnot = toffoli
        -- 000 001 010 011 100 101 110 111
        input  = qBitCreate [0, 0, 0, 0, 0, 0, 1, 0] -- |110>
        measured = qObserve $ toffoli `apply` input 
    in
        do
            putStrLn $ "\nQAdder"
            putStrLn $ "Theorical probabilities :"
            putStrLn $ "|R> = " ++ show input
            putStrLn $ "After measuring"
            putStrLn $ qBitGraph measured 
