{-# LANGUAGE PatternGuards #-}
module Utils.Function where

both f (x,y) = (f x, f y)

with f = \x -> (x, f x)

under a b = \x y -> a (b x y)

-- Numerical functions

affine1d (toA,toB) (fromA,fromB) x = x1
    where
     x0 = (x - toA)/(toB-toA)
     x1 = x0*(fromB-fromA) + fromA

mkFst f a = (f a, a)
mkSnd f a = (a, f a)


-- For Ord. Find a proper location for these

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy op a b | LT <- op a b = a
             | EQ <- op a b = a
             | GT <- op a b = b

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy op a b | LT <- op a b = b
             | EQ <- op a b = a
             | GT <- op a b = a
