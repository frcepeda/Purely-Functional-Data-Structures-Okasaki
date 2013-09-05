{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tree where

import Set

data Tree a = E
            | Tree a (Tree a) (Tree a)
            deriving (Eq, Show)

instance Ord a => Set Tree a where
    empty = E

    member x E = False
    member x (Tree e l r)
        | x == e    = True
        | x < e     = member x l
        | otherwise = member x r

    insert x E = Tree x E E
    insert x t@(Tree e l r)
        | x == e    = t
        | x < e     = Tree e (insert x l) r
        | otherwise = Tree e l (insert x r)
