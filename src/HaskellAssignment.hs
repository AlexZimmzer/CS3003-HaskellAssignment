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
