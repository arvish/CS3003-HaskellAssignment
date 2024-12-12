module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq

instance Show Found where
  show (Match index) = "(Match " ++ show index ++ ")"
  show NoMatch       = "NoMatch"

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
palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse candidate
