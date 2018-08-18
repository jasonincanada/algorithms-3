{- Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #3.1, 3.2 - Huffman coding tree depths

   Remarks:  - naive sorting
             - don't really need a carrier type for the tree, we're only using the weights

   Author: Jason Hooper
-}

import Data.List (sortBy)
import Data.Ord  (comparing)

data Tree = Leaf Integer
            | Branch (Tree) Integer (Tree)
            deriving (Show)

weight :: Tree -> Integer
weight (Leaf  x)      = x
weight (Branch _ x _) = x

merge :: Tree -> Tree -> Tree
merge l r = Branch l (weight l + weight r) r

-- This is somewhat inefficient since it is sorting the list anew in each
-- iteration. Better would be a min-heap or similar structure, but for
-- this data set, this will suffice.  This assumes at least 2 leaves.
buildTree :: [Tree] -> Tree
buildTree leaves = go $ sort leaves
  where go (l:r:[])   = merge l r
        go (l:r:rest) = go $ sort (merge l r : rest)

        sort :: [Tree] -> [Tree]
        sort = sortBy (comparing weight)

measure :: Tree -> (Int -> Int -> Int) -> Int
measure (Leaf _)       _ = 0
measure (Branch l _ r) f = f (measure l f + 1) (measure r f + 1)

main :: IO ()
main = do
  file <- readFile "huffman.txt"
  let probs = map read (tail $ lines file) :: [Integer]
  let tree  = buildTree $ map Leaf probs
  print $ (measure tree min, measure tree max)

