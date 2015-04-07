import Data.List (maximumBy, tails)
import Data.Function (on)
import System.Environment

-- smush words together by combining overlapping sections

{- examples
-- apple + please -> applease
-- apple + apple  -> apple
-- abc + dabc     -> abcdabc (the first one doesn't overlap the second one)
-}

{-
Usage:
  $ runhaskell smush.hs word1 word2 word3 ...
-}

startsWith [] _ = True
startsWith _ [] = False
startsWith (p:ps) (w:ws)
  | p == w = startsWith ps ws
  | otherwise = False

maxOverlap xs ys =
  let overlaps = [x | x <- tails xs, startsWith x ys]
  in maximumBy (compare `on` length) overlaps

smush w1 w2 =
  let overlap = maxOverlap w1 w2
      prefix = take (length w1 - length overlap) w1
      suffix = drop (length overlap) w2
  in prefix ++ overlap ++ suffix

smushWords [] = []
smushWords (w:ws) =
  smush w (smushWords ws)

main = do
  words <- getArgs
  putStrLn $ smushWords words
