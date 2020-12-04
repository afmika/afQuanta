module QMatrix where

import QVector

import Data.Complex
import Data.List

-- helpers
roundDec :: Int -> Float -> Float
roundDec d n = up / pp
	where
		pp   = 10^d
		nup  = n * pp
		up   = fromInteger (round nup)

formatComplex :: Complex Float -> String
formatComplex c = 
	let
		a  = roundDec 2 (realPart c)
		b  = roundDec 2 (imagPart c)
		sg = if b < 0 then " - " else " + "
	in
		(show a) ++ sg ++ "i" ++ (show b)

isSquare xs = 
	let
		row_len  = length xs
		diff_len = \len -> (row_len - len)
		to_check = map diff_len [length r | r <- xs]
	in
		(sum to_check) == 0


posListAt :: [[(Complex Float)]] -> Int -> Int -> (Complex Float)
posListAt xs y x = (xs !! y) !! x

-- functions
data QMatrix = QMatrix [[Complex Float]] deriving (Show, Eq)


qMat :: [[Complex Float]] -> QMatrix
qMat xs
	| isSquare xs = (QMatrix xs) 
	| otherwise   = error "The given matrix should be squared"

qZero :: Int -> QMatrix
qZero n = 
	QMatrix [[0 | n <- idx] | m <- idx] 
	where 
		idx = [1 .. n]

qEye :: Int -> QMatrix
qEye n = 
	let
		d    = \i j -> (if i == j then 1 else 0)
		idx  = [1 .. n]
		mrow = \m -> [ d n m | n <- idx]
	in
		QMatrix [mrow m | m <- idx]

qAt :: QMatrix -> Int -> Int -> (Complex Float)
qAt (QMatrix xs) y x = (posListAt xs) y x

qDim :: QMatrix -> Int
qDim (QMatrix xs) = length xs

qAsList :: QMatrix -> [[Complex Float]]
qAsList (QMatrix xs) = xs

-- ex qMap (\value -> ... ) $ yourMatrix
qMap :: (Complex Float -> Complex Float) -> QMatrix -> QMatrix
qMap func (QMatrix mat) = 
	let
		index = [0 .. (length mat - 1)]
	in
		QMatrix [
			map func $ (mat !! y) | y <- index
		]

-- ex qMapAll (\value row col -> ... ) $ yourMatrix
qMapAll :: (Complex Float -> Int -> Int -> Complex Float) -> QMatrix -> QMatrix
qMapAll func (QMatrix mat) = 
	let
		index = [0 .. (length mat - 1)]
	in
		QMatrix [
			[ func ((qAt (QMatrix mat)) y x) y x | x <- index ] | y <- index
		]


qShow (QMatrix xs) =
	let
		n     = qDim (QMatrix xs)
		index = [0 .. (n-1)]
		mrow  = \r -> [ (posListAt xs) r c | c <- index ]
		joinCols = \row -> intercalate "   " (map formatComplex $ row)
	in
		do {
			putStrLn ( intercalate " x " [show n, show n] );
			putStrLn $ unlines $ [ joinCols (mrow r) | r <- index]
		}


times :: Float -> QMatrix -> QMatrix
n `times` mat = qMap (\v -> (n :+ 0) * v) mat

-- Ex qMat [[1,2,3],[3,4,5],[3,1,1]] `dot` qMat [[1,0,2],[1,-1,3],[5,3,8]]
dot :: QMatrix -> QMatrix -> QMatrix
(QMatrix a) `dot` (QMatrix b)
	| qDim (QMatrix a) /= qDim (QMatrix b) = error "size a /= size b" 
	| otherwise = 
		let
			mA = (QMatrix a)
			mB = (QMatrix b)
			sz = qDim mA
			index = [0 .. (sz-1)]
			pA = qAt mA
			pB = qAt mB
			takeVDot = \rowA colB -> sum [ (pA rowA k) * (pB k colB) | k <- index ]
		in
			QMatrix [ 
				[ takeVDot y x | x <- index ] | y <- index 
			]