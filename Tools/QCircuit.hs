----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QCircuit where
import Data.List
import QBit

data QCircuit = QCircuit [QBit] deriving (Eq)

qIsSingleQubit :: QBit -> Bool
qIsSingleQubit qbit = qBitDim qbit == 2

qAllSingleQubit :: [QBit] -> Bool
qAllSingleQubit xs = foldl (&&) True (map qIsSingleQubit xs)

qCircuitCreate :: [QBit] -> QCircuit
qCircuitCreate xs
    | not $ qAllSingleQubit xs = error "Each input qubit must have 2 components only"
    | otherwise                = QCircuit xs 


qCircuitShow (QCircuit xs) = 
    intercalate "\n" (map show xs)