{-#LANGUAGE ForeignFunctionInterface, TypeFamilies#-}
#include "cvWrapLEO.h"

module CV.Drawing(ShapeStyle(Filled,Stroked),circle,putTextOp,circleOp,fillOp
              ,floodfill,drawLinesOp,lineOp,Drawable,drawLines,rectangle
              ,rectOp,rectOpS,fillPolyOp,fillPoly) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Control.Monad(when)

{#import CV.Image#}

import CV.ImageOp

data ShapeStyle = Filled | Stroked Int
    deriving(Eq,Show)

styleToCV Filled = -1
styleToCV (Stroked w) = fromIntegral w


-- TODO: The instances in here could be significantly smaller..
class Drawable a b where
    type Color a b :: * 
    putTextOp :: (Color a b) -> Float -> String -> (Int,Int) -> ImageOperation a b
    lineOp :: (Color a b)   -> Int -> (Int,Int) -> (Int,Int) -> ImageOperation a b
    circleOp :: (Color a b) -> (Int,Int) -> Int -> ShapeStyle -> ImageOperation a b
    rectOp   :: (Color a b) -> Int -> (Int,Int) -> (Int,Int)  -> ImageOperation a b
    fillPolyOp :: (Color a b) -> [(Int,Int)] -> ImageOperation a b

instance Drawable RGB D32 where
    type Color RGB D32 = (D32,D32,D32)
    putTextOp (r,g,b) size text (x,y)  = ImgOp $ \img -> do
                                   withGenImage img $ \cimg ->
                                    withCString text $ \(ctext) ->
                                    {#call wrapDrawText#} cimg ctext (realToFrac size) 
                                        (fromIntegral x) (fromIntegral y) 
                                        (realToFrac r) (realToFrac g) (realToFrac b) 

    lineOp (r,g,b) t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawLine#} img (fromIntegral x) (fromIntegral y) 
                                                        (fromIntegral x1) (fromIntegral y1) 
                                                        (realToFrac r) (realToFrac g) 
                                                        (realToFrac b) (fromIntegral t) 
    
    circleOp (red,g,b) (x,y) r s = ImgOp $ \i -> do
                        when (r>0) $ withGenImage i $ \img -> 
                              ({#call wrapDrawCircle#} img (fromIntegral x) (fromIntegral y) 
                                                           (fromIntegral r) (realToFrac red) 
                                                           (realToFrac g) (realToFrac b)
                                                           $ styleToCV s)
    rectOp (r,g,b) t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawRectangle#} img (fromIntegral x)
                               (fromIntegral y) (fromIntegral x1) (fromIntegral y1)
                               (realToFrac r) (realToFrac g)(realToFrac b)(fromIntegral t)
    fillPolyOp (r,g,b) pts = ImgOp $ \i -> do
                             withImage i $ \img -> do
                                  let (xs,ys) = unzip pts
                                  xs' <- newArray $ map fromIntegral xs
                                  ys' <- newArray $ map fromIntegral  ys
                                  {#call wrapFillPolygon#} img 
                                       (fromIntegral $ length xs) xs' ys' 
                                   (realToFrac r) (realToFrac g) (realToFrac b) 
                                  free xs'
                                  free ys'


instance Drawable GrayScale D32 where
    type Color GrayScale D32 = D32

    putTextOp color size text (x,y)  = ImgOp $ \img -> do
                                   withGenImage img $ \cimg ->
                                    withCString text $ \(ctext) ->
                                    {#call wrapDrawText#} cimg ctext (realToFrac size) 
                                        (fromIntegral x) (fromIntegral y)   
                                        (realToFrac color) (realToFrac color) (realToFrac color) 

    lineOp c t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawLine#} img (fromIntegral x) (fromIntegral y) 
                                                        (fromIntegral x1) (fromIntegral y1) 
                                                        (realToFrac c) (realToFrac c) 
                                                        (realToFrac c) (fromIntegral t) 

    circleOp c (x,y) r s = ImgOp $ \i -> do
                        when (r>0) $ withGenImage i $ \img -> 
                              ({#call wrapDrawCircle#} img (fromIntegral x) (fromIntegral y) 
                                                           (fromIntegral r) 
                                                           (realToFrac c) (realToFrac c) (realToFrac c) 
                                                           $ styleToCV s)

    rectOp c t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawRectangle#} img (fromIntegral x)
                               (fromIntegral y) (fromIntegral x1) (fromIntegral y1)
                               (realToFrac c)(realToFrac c)(realToFrac c) (fromIntegral t)

    fillPolyOp c pts = ImgOp $ \i -> do
                             withImage i $ \img -> do
                                  let (xs,ys) = unzip pts
                                  xs' <- newArray $ map fromIntegral xs
                                  ys' <- newArray $ map fromIntegral  ys
                                  {#call wrapFillPolygon#} img 
                                       (fromIntegral $ length xs) xs' ys' 
                                   (realToFrac c) (realToFrac c) (realToFrac c) 
                                  free xs'
                                  free ys'


rectOpS c t pos@(x,y) (w,h) = rectOp c t pos (x+w,y+h)

fillOp :: (Int,Int) -> D32 -> D32 -> D32 -> Bool -> ImageOperation GrayScale D32
fillOp (x,y) color low high floats = 
    ImgOp $ \i -> do
      withImage i $ \img -> 
        ({#call wrapFloodFill#} img (fromIntegral x) (fromIntegral y)
            (realToFrac color) (realToFrac low) (realToFrac high) (toCINT $ floats))
    where
     toCINT False = 0
     toCINT True  = 1

-- Shorthand for single drawing operations. You should however use #> and <## in CV.ImageOp 
-- rather than these

--line color thickness start end i = 
--    operate (lineOp color thickness start end ) i

rectangle color thickness a b i = 
    operate (rectOp color thickness a b ) i

fillPoly c pts i = operate (fillPolyOp c pts) i

drawLinesOp color thickness segments = 
    foldl (#>) nonOp 
     $ map (\(a,b) -> lineOp color thickness a b) segments

drawLines img color thickness segments = operateOn img
                    (drawLinesOp color thickness segments)

circle center r color s i = unsafeOperate (circleOp color center r s) i

floodfill (x,y) color low high floats = 
    unsafeOperate (fillOp (x,y) color low high floats) 

            

