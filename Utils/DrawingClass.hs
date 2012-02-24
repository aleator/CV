{-#LANGUAGE TypeFamilies, MultiParamTypeClasses#-}
module Utils.DrawingClass where

class Draws a b where
    draw :: a -> b
