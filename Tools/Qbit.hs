----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QBit where

import QMath
import QVector
import QRandom

import Data.Complex
import Data.List
import System.IO.Unsafe ( unsafePerformIO )

data QBit = QBit (QVector) deriving (Eq)

qBit :: QVector -> QBit
qBit vec
    | not (qHasDimPowOf2 vec) || ((qVDim vec) == 1) = error "Vec dimension should be a power of 2 and /= 1" 
    | not (qIsVUnitary vec)   = error "Vec should be unitary"
    | otherwise = QBit vec

qBitVec :: QBit -> QVector
qBitVec (QBit v) = v

qBitFromVal :: Int -> Int -> QBit
qBitFromVal reg_size val
    | (2^reg_size) < val  = error "Dim should be large enough to encode val"
    | otherwise   = qBit $ qVec [ 
                        if i == val then (1:+0) else (0:+0) | i <- [0 .. (2^reg_size) - 1]
                    ]

qSingleBit :: Int -> QBit
qSingleBit = qBitFromVal 1

qBitCreate :: [Complex Double] -> QBit
qBitCreate xs = qBit $ qVec xs

qBitAsList :: QBit -> [Complex Double]
qBitAsList (QBit (QVector xs)) = xs

qBitDim :: QBit -> Int
qBitDim qb = qVDim $ qBitVec qb

-- inner product between two qubits
qdot :: QBit -> QBit -> Complex Double
(QBit a) `qdot` (QBit b) = a `vdot` b

qProbabilities :: QBit -> [Double]
qProbabilities (QBit (QVector xs)) = [ realPart ((abs m)^2) | m <- xs]

-- checks if all probabilities are equal (the index should be uniformly picked)
qEqualProbabilities :: QBit -> Bool
qEqualProbabilities qb = 
    let
        probs = qProbabilities qb
        first = probs !! 0
        func  = \a b -> if areEqual a b then 1 else 0 
    in
        (length probs) == (sum [ func first p | p <- probs])

-- tensor product between two qubits
-- We can use this operator to combine two qubits
-- Assuming the two operands are not entangled
combine :: QBit -> QBit -> QBit
(QBit (QVector a)) `combine` (QBit (QVector b)) = 
    let
        ta = transpose [a]
        tb = transpose [b]
        kr = kroneker_prod ta tb
    in
        qBit $ qVec $ map (\i -> i !! 0 ) kr



-- IO Operations (actions)

qIOObserve :: QBit -> IO QBit
qIOObserve (QBit (QVector xs)) = do
    picked <- 
            if (qEqualProbabilities $ QBit (QVector xs)) then 
                qRandPickIndexUniform (qProbabilities $ QBit (QVector xs))
            else
                qRandPickIndex (qProbabilities $ QBit (QVector xs)) 0 False

    let len   = length xs
    let index = [0 .. len - 1]
    let pickMe = \i -> if i /= picked then (0 :+ 0) else (1 :+ 0)
    return $ qBit $ qVec [ pickMe i | i <- index ]


-- IO QBit --> QBit
qObserve qb = unsafePerformIO $ qIOObserve qb

-- Output format

qBitGraph :: QBit -> String
qBitGraph qb =
    let
        max_height = 30.0
        ps    = qProbabilities qb
        probs = map (\x -> x * max_height) $ ps
        len   = length probs
        toprc = \i -> roundDec 2 ((ps !! i) * 100)

        format_idxb2 = \i -> formatStrNumDigits (show (dec2BinNum i)) (fromEnum $ log (fromIntegral len) / log 2)
        (d, r) = (len - 1) `divMod` 10
        format_idx   = \i -> formatStrNumDigits (show i) (if d > 0 then (d+1) else 1)

        tob2  = \i -> " |" ++ (format_idxb2 i) ++ ">"
        putLn = \i p -> 
                "[" ++ (format_idx i) ++ "]" ++ (tob2 i) 
                ++ " | " ++ intercalate "" (take p $ repeat ":")
                ++ " " ++ (show $ toprc i ) ++ "%"
    in
        intercalate "\n" [ putLn i (round (probs !! i)) | i <- [0 .. len - 1] ]

-- show override
instance Show QBit where
  show (QBit (QVector xs)) = 
      let
          len = length xs
          format_idxb2 = \i -> formatStrNumDigits (show (dec2BinNum i)) (fromEnum $ log (fromIntegral len) / log 2)
          fmter = \v -> (formatComplexStrict v)
          right = \v -> " |" ++ v ++ ">"
          res   = [ (fmter $ xs !! i) ++ right (format_idxb2 i) | i <- [0 .. len - 1]]
      in
          intercalate " + " res