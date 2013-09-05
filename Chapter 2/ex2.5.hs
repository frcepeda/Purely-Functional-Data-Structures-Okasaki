{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

class FiniteMap m k a where
    empty :: m k a
    bind :: k -> a -> m k a -> m k a
    find :: k -> m k a -> Maybe a
    
data TMap k a = E
              | TMap (k,a) (TMap k a) (TMap k a)
              deriving (Eq, Show)

instance Ord k => FiniteMap TMap k a where
    empty = E

    find k E = Nothing
    find k t = find' t k t
        where find' (TMap (k,a) _ _) x E = if k == x then Just a else Nothing
              find' t x t'@(TMap (k,a) l r)
                   | x <= k = find' t' x l
                   | x > k  = find' t x r

    bind k a E = TMap (k,a) E E
    bind k a t@(TMap (k',a') l r)
        | k == k'   = TMap (k,a) l r
        | k < k'    = TMap (k',a') (bind k a l) r
        | otherwise = TMap (k',a') l (bind k a r)
