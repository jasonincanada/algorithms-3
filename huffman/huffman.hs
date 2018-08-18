{- Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #3.1, 3.2 - Huffman coding tree depths

   Remarks:  - naive sorting
             - don't really need a carrier type for the tree, we're only using the weights

   Author: Jason Hooper
-}

import Data.List (sortBy)
import Data.Ord  (comparing)

data Tree a = Leaf a Integer
            | Branch (Tree a) Integer (Tree a)
            deriving (Show)

-- Convert the input list to Leaf nodes, which will be progressively
-- merged into the final tree
initialize :: [(a, Integer)] -> [Tree a]
initialize = map (\(c, i) -> Leaf c i)

weight :: Tree a -> Integer
weight (Leaf   _ x  ) = x
weight (Branch _ x _) = x

merge :: Tree a -> Tree a -> Tree a
merge l r = Branch l (weight l + weight r) r

-- This is somewhat inefficient since it is sorting the list anew in each
-- iteration. Better would be a min-heap or similar structure, but for
-- this data set, this will suffice
buildTree :: [Tree a] -> Tree a
buildTree leaves = go $ sort leaves
  where go (l:r:[])   = merge l r
        go (l:r:rest) = go $ sort $ merge l r : rest

        sort :: [Tree a] -> [Tree a]
        sort = sortBy (comparing weight)

-- Find the maximum depth of a tree
depth :: Tree a -> Int
depth (Leaf _   _  ) = 0
depth (Branch l _ r) = max (depth l + 1) (depth r + 1)

-- Find the minimum depth of a tree
mepth :: Tree a -> Int
mepth (Leaf _   _  ) = 0
mepth (Branch l _ r) = min (mepth l + 1) (mepth r + 1)

main :: IO ()
main = do
  file <- readFile "huffman.txt"
  let probs = map read (tail $ lines file) :: [Integer]
  let init  = initialize $ zip [1..] probs
  let tree  = buildTree init
  print $ (mepth tree, depth tree)

