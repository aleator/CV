{-#LANGUAGE TypeFamilies#-}
module Utils.GeometryClass where

class Point2D a where
   type EL a :: *
   pt :: a -> (EL a, EL a)

