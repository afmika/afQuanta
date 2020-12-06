module QMath where

import Data.List
import Data.Complex
import Control.Applicative 


v_upsilon :: Double
v_upsilon = 1 / 10^6

kroneker_prod ::  [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
kroneker_prod xs ys =
    let f = fmap . fmap . (*) -- Multiplication by n over list of lists
    in fmap concat . transpose =<< fmap (`f` ys) <$> xs