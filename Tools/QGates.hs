module QGates where

import Data.Complex
import QMatrix

hadamard2 = (1 / sqrt 2) `times` QMatrix [
			[1, 1],
			[1, -1]
		]