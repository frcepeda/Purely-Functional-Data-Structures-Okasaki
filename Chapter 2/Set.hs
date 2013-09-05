{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Set where

class Set s a where
    empty :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool
