module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (length, (..), concatMap, (:), null, filter)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Foldable (foldl)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

isEven 0 = true
isEven n = if n < 0 then false else isEven (n-2)

numEven :: Array Int -> Int
numEven arr = 
  if null arr then
    0
  else
    if unsafePartial head arr # isEven
      then 1 + (numEven (unsafePartial tail arr))
      else numEven (unsafePartial tail arr)

infix 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (factors n # length) == 1 

cartesianProd :: Array Int -> Array Int -> Array (Array Int)
cartesianProd a1 a2 = do
  a1' <- a1
  a2' <- a2 
  pure [a1', a2']

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ (a*a) + (b*b) == (c*c)
  pure [a,b,c]

--I have no clue how to do this
factorizations :: Int -> Array (Array Int)
factorizations n = 
  [[]]

all :: Array Boolean -> Boolean
all xs = foldl (\agg x -> agg && x) true xs