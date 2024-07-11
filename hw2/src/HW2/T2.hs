module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List.NonEmpty (NonEmpty (..), (<|))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn del = foldr checkSplit ([] :| [])
  where 
    checkSplit ch l@(h :| t) = if ch == del
      then [] <| l
      else (ch : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith del (h :| t) = h ++ foldr (\ch e -> (del : ch) ++ e) [] t
