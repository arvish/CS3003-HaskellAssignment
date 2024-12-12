module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------

data Found = Match Int | NoMatch deriving (Eq)

findFirst :: (a -> Bool) -> [a] -> Found
findFirst predicate xs = go 0 xs
  where
    go _ []     = NoMatch
    go i (y:ys)
      | predicate y = Match i
      | otherwise    = go (i + 1) ys

------------------------------------------------
-- palindrome
------------------------------------------------

palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse candidate
