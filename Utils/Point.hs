{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
module Utils.Point where
import Utils.GeometryClass

instance Point2D (Int,Int) where
   type ELP (Int,Int) = Int
   pt = id

instance Point2D (Double,Double) where
   type ELP (Double,Double) = Double
   pt = id

type Pt a = (a,a)

instance Num a => Num (Pt a) where
    (x1,x2) + (y1,y2) = (x1+y1,x2+y2)
    (x1,x2) * (y1,y2) = (x1*y1,x2*y2)
    (x1,x2) - (y1,y2) = (x1-y1,x2-y2)
    negate (x1,x2) = (negate x1,negate x2)
    abs (x1,x2) = (abs x1, abs x2) -- Not really mathematical, but the type must be same
    signum (x1,x2) = (signum x1, signum x2) -- Ditto
    fromInteger x = (fromInteger x,fromInteger x) -- As well

norm2 :: (Num a) => Pt a -> a
norm2 (a,b) = a*a+b*b

norm = sqrt . norm2

(a,b) >/ (c,d) = (a `div` c, b `div` d)
