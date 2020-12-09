----------------------------------------
-- author : afmika
-- email  : afmichael73@gmail.com
----------------------------------------

module QMatrix where

import QVector
import QMath

import Data.Complex
import Data.List

isSquare xs = 
	let
		row_len  = length xs
		diff_len = \len -> (row_len - len)
		to_check = map diff_len [length r | r <- xs]
	in
		(sum to_check) == 0


posListAt :: [[(Complex Double)]] -> Int -> Int -> (Complex Double)
posListAt xs y x = (xs !! y) !! x

-- defs
data QMatrix = QMatrix [[Complex Double]] deriving (Eq)

qMat :: [[Complex Double]] -> QMatrix
qMat xs
	| isSquare xs = (QMatrix xs) 
	| otherwise   = error "The given matrix must be square"

qZero :: Int -> QMatrix
qZero n = 
	qMat [[0 | n <- idx] | m <- idx] 
	where 
		idx = [1 .. n]

qEye :: Int -> QMatrix
qEye n = 
	let
		d    = \i j -> (if i == j then 1 else 0)
		idx  = [1 .. n]
		mrow = \m -> [ d n m | n <- idx]
	in
		qMat [mrow m | m <- idx]

qAt :: QMatrix -> Int -> Int -> (Complex Double)
qAt (QMatrix xs) y x = (posListAt xs) y x

qDim :: QMatrix -> Int
qDim (QMatrix xs) = length xs

qAsList :: QMatrix -> [[Complex Double]]
qAsList (QMatrix xs) = xs

kprod :: QMatrix -> QMatrix -> QMatrix
(QMatrix a) `kprod` (QMatrix b) = qMat $ kroneker_prod a b


-- ex qMap (\value -> ... ) $ yourMatrix
qMap :: (Complex Double -> Complex Double) -> QMatrix -> QMatrix
qMap func (QMatrix mat) = 
	let
		index = [0 .. (length mat - 1)]
	in
		qMat [
			map func $ (mat !! y) | y <- index
		]

qMapDouble :: (Complex Double -> Complex Double -> Complex Double) -> QMatrix -> QMatrix -> QMatrix
qMapDouble func a b
	| qDim a /= qDim b = error "operands must be compatible"
	| otherwise        =
		let
			index = [0 .. qDim a - 1]
			pA    = qAt a
			pB    = qAt b
		in
			qMat [
				[ func (pA y x) (pB y x) | x <- index] | y <- index
			]

-- ex qMapAll (\value row col -> ... ) $ yourMatrix
qMapAll :: (Complex Double -> Int -> Int -> Complex Double) -> QMatrix -> QMatrix
qMapAll func (QMatrix mat) = 
	let
		index = [0 .. (length mat - 1)]
	in
		qMat [
			[ func ((qAt (QMatrix mat)) y x) y x | x <- index ] | y <- index
		]

-------------------------------------------------------
-- I expect you to understand this.... oh future me  --
-------------------------------------------------------

times :: Double -> QMatrix -> QMatrix
n `times` mat = qMap (\v -> (n :+ 0) * v) mat

ctimes :: (Complex Double) -> QMatrix -> QMatrix
n `ctimes` mat = qMap (\v -> n*v) mat

dot :: QMatrix -> QMatrix -> QMatrix
a `dot` b
		| qDim a /= qDim b = error "size a /= size b" 
		| otherwise = 
			let
				sz    = qDim a
				index = [0 .. sz - 1]
				pA = qAt a
				pB = qAt b
				takeVDot = \rowA colB -> sum [ (pA rowA k) * (pB k colB) | k <- index ]
			in
				qMat [ 
					[ takeVDot y x | x <- index ] | y <- index 
				]

mtimes :: QMatrix -> QVector -> QVector
a `mtimes` b
		| qDim a /= qVDim b = error "operands must be compatible (same row dimension)"
		| otherwise         =
			let
				sz    = qDim a
				ma    = qAsList a
				index = [0 .. sz - 1]
				prod  = \x y -> x * y
				a_row = \r -> ma !! r
				b_col = qVAsList b
			in
				qVec $ map sum [ zipWith prod (a_row r) b_col | r <- index ]

qIsZero :: QMatrix -> Bool
qIsZero (QMatrix m) = 
		let
			t   = abs $ sum [sum (m !! i) | i <- [0 .. length m - 1] ]
		in 
			realPart t <= v_upsilon && imagPart t <= v_upsilon

qTranspose :: QMatrix -> QMatrix
qTranspose (QMatrix xs) = qMat $ transpose xs

-- M^dagger = Adjoint matrix (conjugate transpose)
qConjugateTranspose :: QMatrix -> QMatrix
qConjugateTranspose mt = qMap conjugate $ qTranspose mt

-- Hermitian matrix --> M^dagger = M
qIsHermitian :: QMatrix -> Bool
qIsHermitian mt = qIsZero $ mt `mdelta` qConjugateTranspose mt

-- matA - matB
mdiff :: QMatrix -> QMatrix -> QMatrix
a `mdiff` b = qMapDouble (\x y -> x - y) a b

-- matA + matB
mplus :: QMatrix -> QMatrix -> QMatrix
a `mplus` b = qMapDouble (\x y -> x + y) a b

-- abs (matA - matB)
mdelta :: QMatrix -> QMatrix -> QMatrix
a `mdelta` b = qMapDouble (\x y -> abs(x - y)) a b


-- Unitary matrix --> M^dagger M = I <=> M^dagger = M^-1
qIsUnitary :: QMatrix -> Bool
qIsUnitary mt = 
	let
		id       = qEye (qDim mt)
		m_dagger = qConjugateTranspose mt
	in
		--  M^dagger M = I
		-- (M^dagger M) - I = 0 
		qIsZero $ (m_dagger `dot` mt) `mdelta` id


instance Show QMatrix where
	show (QMatrix xs) =
		let
			n     = qDim (QMatrix xs)
			index = [0 .. (n-1)]
			mrow  = \r -> [ (posListAt xs) r c | c <- index ]
			joinCols = \row -> intercalate "   " (map formatComplex $ row)
		in
			( intercalate " x " [show n, show n] ) 
			++ "\n" ++
			unlines [ joinCols (mrow r) | r <- index]
