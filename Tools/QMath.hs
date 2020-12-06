module QMath where

import Data.List
import Data.Complex
import Control.Applicative 

kroneker_prod ::  [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
kroneker_prod xs ys =
    let f = fmap . fmap . (*) -- Multiplication by n over list of lists
    in fmap concat . transpose =<< fmap (`f` ys) <$> xs