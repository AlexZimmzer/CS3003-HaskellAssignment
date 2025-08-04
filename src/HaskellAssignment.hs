module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst needle haystack = go 0 haystack
  where
    go [] _  = NoMatch -- if not found, return NoMatch
    go i (x:xs) -- steps through, keeping track of i
      | needle x = Match i -- if found at index i
      | otherwise = go (i+1) xs -- keep going
      
------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse candidate

------------------------------------------------
-- mergesort
------------------------------------------------
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ []  = []
mergesort _ [x] = [x]
mergesort cmp xs =
  let (left, right) = splitAt (length xs `div` 2) xs
  in merge cmp (mergesort cmp left) (mergesort cmp right)

-- merge helper
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge cmp (x:xs) (y:ys)
  | cmp x y   = x : merge cmp xs (y:ys)
  | otherwise = y : merge cmp (x:xs) ys

------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = RunLength Int Char
  deriving (Show, Eq)

runLengthEncode :: String -> [RunLength]
runLengthEncode [] = []
runLengthEncode (x:xs) = encode 1 x xs
  where
    encode n c [] = [RunLength n c]
    encode n c (y:ys)
      | y == c    = encode (n + 1) c ys
      | otherwise = RunLength n c : encode 1 y ys
