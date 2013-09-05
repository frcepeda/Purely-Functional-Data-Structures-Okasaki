{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tree where

import Set

data Tree a = E
            | Tree a (Tree a) (Tree a)
            deriving (Eq, Show)

instance Ord a => Set Tree a where
    empty = E

-- exercise 2.2
    member x E = False
    member x t@(Tree e _ _) = member' e x t
        where member' c x E = x == c
              member' c x (Tree e l r)
                   | x <= e = member' e x l
                   | x > e  = member' c x r

-- exercise 2.3
    insert x t = if not (member x t) then insert' x t else t
                 where insert' x E = Tree x E E
                       insert' x t@(Tree e l r)
                           | x < e     = Tree e (insert' x l) r
                           | otherwise = Tree e l (insert' x r)
    -- close enough? It's still O(log n)...

-- exercise 2.4 was joining 2.2 and 2.3.
