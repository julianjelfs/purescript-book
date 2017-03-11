module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, (:), null)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

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
