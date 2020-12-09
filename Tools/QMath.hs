module QMath where

import Data.List
import Data.Complex
import Control.Applicative 


v_upsilon :: Double
v_upsilon = 1 / 10^6

-- helpers
roundDec :: Int -> Double -> Double
roundDec d n = up / pp
	where
		pp   = 10^d
		nup  = n * pp
		up   = fromInteger (round nup)

formatComplex :: Complex Double -> String
formatComplex c = 
	let
		a  = roundDec 2 (realPart c)
		b  = roundDec 2 (imagPart c)
		sg = if b < 0 then " - " else " + "
	in
		(show a) ++ sg ++ "i" ++ (show $ abs b)


to_base :: [Int] -> Int -> [Int]
to_base xs n
	| n <= 1    = (n:xs)
	| otherwise =
		let
			(d, r) = n `divMod` 2
		in
			to_base (r:xs) d

dec2BinList :: Int -> [Int]
dec2BinList n = to_base [] n

dec2BinNum :: Int -> Int
dec2BinNum n =
	let 
		res = dec2BinList n
		len = length res
	in
		sum [ (res !! i) * 10^i | i <- [0 .. len - 1] ]

kroneker_prod ::  [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
kroneker_prod xs ys =
    let f = fmap . fmap . (*) -- Multiplication by n over list of lists
    in fmap concat . transpose =<< fmap (`f` ys) <$> xs