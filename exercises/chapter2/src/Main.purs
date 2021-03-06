module Main where

import Prelude ((+), (*))
import Control.Monad.Eff.Console (logShow)
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r =
    r * r * pi


main = logShow (diagonal 3.0 4.0)
