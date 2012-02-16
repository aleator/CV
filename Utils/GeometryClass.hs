{-#LANGUAGE TypeFamilies,FlexibleInstances #-}
module Utils.GeometryClass where

import Utils.Rectangle
import Utils.Point

class Point2D a where
   type ELP a :: *
   pt :: a -> (ELP a, ELP a)
   toPt :: (ELP a,ELP a) -> a

instance Point2D (Int,Int) where
   type ELP (Int,Int) = Int
   pt = id
   toPt = id

instance Point2D (Double,Double) where
   type ELP (Double,Double) = Double
   pt = id
   toPt = id

convertPt :: (Point2D a, Point2D b, ELP a ~ ELP b) => a -> b
convertPt = toPt . pt

class BoundingBox a where
   type ELBB a :: *
   bounds :: a -> Rectangle (ELBB a)

instance BoundingBox (Rectangle a) where
   type ELBB (Rectangle a) = a
   bounds = id

class Line2D a where
   type ELL a :: *
   offsetAngle :: a -> (ELL a, Double)

class LineSegment a where
   type ELS a :: *
   startEnd :: a -> ((ELS a, ELS a),(ELS a, ELS a))

