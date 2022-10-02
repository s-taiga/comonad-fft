module Main where

import qualified MyLib (someFunc)

import FilterComonad
import Data.Complex (Complex(..))
import GHC.Float (int2Double)

main :: IO ()
main = do
  -- let tz = fromList2TZ [1..10]
  -- let filtered = applyFilter neighborMean tz
  -- print filtered
  let xs = IL [(i, cos (2 * pi / 16 * int2Double (i + 3)) :+ 0) | i <- [0..15]]
  print xs
  print $ applyFilter dft xs
  print $ applyFilter fft xs