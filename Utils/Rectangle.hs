module Utils.Rectangle where
import Test.LazySmallCheck

import Utils.Point
import Control.DeepSeq

data Rectangle a = Rectangle  !a !a !a !a deriving (Eq,Show)


a `s` b = rnf a `seq` b

instance (NFData a) => NFData (Rectangle a) where
    rnf (Rectangle a b c d) = (a `s` b `s` c `s` d) `seq` ()

left   (Rectangle x y w h) = x
right  (Rectangle x y w h) = x+w
top    (Rectangle x y w h) = y
bottom (Rectangle x y w h) = y+h
topLeft  (Rectangle x y w h) = (x,y)
topRight (Rectangle x y w h) = (x+w,y)
bottomLeft (Rectangle x y w h) = (x,y+h)
bottomRight (Rectangle x y w h) = (x+w,y+h)
vertices r = [topLeft r, topRight r, bottomLeft r, bottomRight r]
rSize (Rectangle x y w h) = (w,h)
rArea r = let (w,h) = rSize r in (w*h)

center (Rectangle x y w h) = (x+w/2,y+h/2)
centerI (Rectangle x y w h) = (x+w`div`2,y+h`div`2)

-- TODO: Add documentation #Cleanup

instance (Num a, Ord a , Serial a) => Serial (Rectangle a) where
    series = cons4 $ \a b c d -> mkRectangle (a,b) (c,d)

-- | Create rectangle around point (x,y)
around (x,y) (w,h) = mkRectangle (x',y') (w,h)
    where (x',y') = (x-(w/2),y-(h/2))

mkRectangle (x,y) (w,h) = Rectangle (x-negW) (y-negH) (abs w) (abs h)
    where
     negH | h<0  = abs h
          | h>=0 = 0
     negW | w<0  = abs w
          | w>=0 = 0

mkRectCorners (x1,y1) (x2,y2) = Rectangle x y w h
 where
    x = min x1 x2
    y = min y1 y2
    w = abs (x1-x2)
    h = abs (y1-y2)

prop_Corners :: (Int,Int) -> (Int,Int) -> Bool
prop_Corners p w = mkRectCorners p (p+w) == mkRectangle p w

mkRec = uncurry mkRectangle

fromPtSize (x,y) (w,h) = Rectangle x y w h

-- | Return rectangle r2 in coordinate system defined by r1
inCoords r1 r2@(Rectangle x y w h) = fromPtSize ((x,y)-topLeft r1,(w,h) )

-- | Return a point in coordinates of given rectangle
inCoords' r1 pt = pt - topLeft r1

-- | Adjust the size of the rectangle to be divisible by 2^n.
enlargeToNthPower n (Rectangle x y w h) = Rectangle x y w2 h2
    where
     (w2,h2) = (pad w, pad h)
     pad x = x + (np - x `mod` np)
     np = 2^n

intersection r1 r2
    = mkRectCorners (max (left r1)   (left r2)
                    ,max (top r1)    (top r2))
                    (min (right r1)  (right r2)
                    ,min (bottom r1) (bottom r2))

propIntersectionArea r1 r2
    = (intersects r1 r2)
       ==> rArea (intersection r1 r2) <= rArea r1 &&
           rArea (intersection r1 r2) <= rArea r2

propIntersectionCommutes r1 r2
    = (intersects r1 r2)
       ==> (intersection r1 r2) == (intersection r2 r1)

intersects rect1 rect2
    = intersect1D (left rect1, right rect1) (left rect2, right rect2) &&
      intersect1D (top rect1, bottom rect1) (top rect2, bottom rect2)

contains a b = left a <= left b
                && top a <= top b
                && bottom a >= bottom b
                && right a >= right b

intersect1D (x,y) (u,w) =
    not $ (x < min u w && y < min u w) || (x > max u w && y > max u w)

prop_intersect1DCommutes a b
    = intersect1D  a b == intersect1D b a

prop_intersectsCommutes sa@(_,(s1,s2)) sb@(b,(s3,s4))
    = intersects (mkRec sa) (mkRec sb) == intersects (mkRec sb) (mkRec sa)

-- | Create a tiling of a rectangles.
tile tilesize overlap r = [mkRectangle ((x,y)-overlap) tilesize
                          | x <- [startx,startx+fst tilesize..endx]
                          , y <- [starty,starty+fst tilesize..endy] ]
    where
     startx = left r-fst overlap
     starty = top  r-snd overlap
     endx = right  r+fst overlap
     endy = bottom r+snd overlap

-- | Scale a rectangle
scale (a,b) (Rectangle x y s1 s2)
    = mkRectangle (round (a*fromIntegral x),round (b*fromIntegral y))
                  (round (a*fromIntegral s1),round (b*fromIntegral s2))


fromInt (Rectangle a b c d)
    = Rectangle (f a) (f b) (f c) (f d)
 where f = fromIntegral

roundR (Rectangle a b c d)
    = Rectangle (f a) (f b) (f c) (f d)
 where f = round

