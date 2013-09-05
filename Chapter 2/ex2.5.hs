{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

class FiniteMap m k a where
    empty :: m k a
    bind :: k -> a -> m k a -> m k a
    lookup :: k -> m k a -> Maybe a
    
data TMap k a = E
              | TMap (k,a) (TMap k a) (TMap k a)
              deriving (Eq, Show)

instance Ord k => FiniteMap TMap k a where
    empty = E

    lookup k E = Nothing
    lookup k t = lookup' t k t
        where lookup' (TMap (k,a) _ _) x E = if k == x then Just a else Nothing
              lookup' t x t'@(TMap (k,a) l r)
                   | x <= k = lookup' t' x l
                   | x > k  = lookup' t x r

    bind k a E = TMap (k,a) E E
    bind k a t@(TMap (k',a') l r)
        | k == k'   = TMap (k,a) l r
        | k < k'    = TMap (k',a') (bind k a l) r
        | otherwise = TMap (k',a') l (bind k a r)
