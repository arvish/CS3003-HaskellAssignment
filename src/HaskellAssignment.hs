module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------

-- The Found data type to represent whether a match has been found, and at which index
data Found = Match Int | NoMatch deriving Eq

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

-- findFirst takes a predicate and a list and returns Found.
-- If the predicate is satisfied by an element, return Match with that element's index.
-- If no element satisfies the predicate, return NoMatch.
findFirst :: (a -> Bool) -> [a] -> Found
findFirst predicate xs = go 0 xs
  where
    go _ []     = NoMatch
    go i (y:ys) = if predicate y
                  then Match i
                  else go (i + 1) ys

------------------------------------------------
-- palindrome
------------------------------------------------

-- A palindrome function that checks if a given string is the same forwards and backwards.
-- The empty string is considered a palindrome.
palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse candidate