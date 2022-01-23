----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module Main where

import QBit
import QGate
import QCircuit

circuit = qCircuitCreate [
        qBitCreate [1, 0],
        qBitCreate [0, 1]
    ]

main :: IO ()
main = do 
    print $ qCircuitShow circuit