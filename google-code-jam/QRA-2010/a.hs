import System.IO
import System.Directory
import Data.List
import System.Environment

-- usage: `runhaskell a inFile` e.g. `runhaskell a A-small-practice.in`
-- output is written to "out.txt"

main = do
  [inFile] <- getArgs
  contents <- readFile inFile
  let items = lines contents
  let cases = tail items
  let result = solve cases
  let indexedCases = ["Case #" ++ (show i) ++ ": " ++ n | (i,n) <- zip [1..] result]
  writeFile "out.txt" $ unlines indexedCases
  putStrLn $ unlines indexedCases

solve :: [String] -> [String]
solve [] = []
solve xs = solveOne (take 3 xs) : solve (drop 3 xs)

readInt n = read n :: Int

join' [] = fail "can't join an empty list"
join' (x:[]) = x
join' (x:xs) = x ++ " " ++ (join' xs)

solveOne :: [String] -> String
solveOne (creditS:itemCountS:pricesS:[]) =
  let credit = readInt creditS
      itemCount = readInt itemCountS
      prices = map readInt (words pricesS)
  -- in join' ["credit",(show credit),"itemCount",(show itemCount),"prices",(show prices)]
  in join' $ map show [x | x <- findSum credit prices]
  {-in join' $ map show (map inc (findSum credit prices))-}

inc x = x + 1
findSum :: (Ord a, Eq a, Enum a, Num a) => a -> [a] -> [a]
findSum n xs =
  let indexedPrices = zip [1..] xs
      tuples = [(i1,i2) | (i1,a) <- indexedPrices, (i2,b) <- indexedPrices, a+b == n, i1 /= i2]
      firstTuple = head tuples
  in (fst firstTuple) : (snd firstTuple) : []
