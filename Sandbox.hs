----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module Main where

import QBit
import QGate
import QCircuit

circuit = qCircuitCreate [
        qSingleBit 0, -- |0>
        qSingleBit 0, -- |0>
        qSingleBit 1, -- |1>
        qSingleBit 0  -- |0>
    ]

main :: IO ()
main = do 
    putStrLn $ qCircuitShow circuit