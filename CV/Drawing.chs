{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"

module CV.Drawing(ShapeStyle(Filled,Stroked),circle,putTextOp,circleOp,fillOp
              ,floodfill,drawLinesOp,lineOp,line,drawLines,rectangle
              ,rectOp,rectOpS,fillPolyOp,fillPoly) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import System.IO.Unsafe

{#import CV.Image#}

import CV.ImageOp

data ShapeStyle = Filled | Stroked Int
    deriving(Eq,Show)

styleToCV Filled = -1
styleToCV (Stroked w) = fromIntegral w

putTextOp size text (x,y) = ImgOp $ \img -> do
                               withGenImage img $ \cimg ->
                                withCString text $ \(ctext) ->
                                {#call wrapDrawText#} cimg ctext size x y   

circleOp (x,y) r c s = ImgOp $ \i -> do
                        let (w,h) = getSize i
                        let tr = r + abs (styleToCV s)
                        if r <1
                         then return () 
                         else withGenImage i $ \img -> 
                              ({#call wrapDrawCircle#} img x y r c 
                                $ styleToCV s)

lineOp c t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawLine#} img x y x1 y1 c t 

fillPolyOp c pts = ImgOp $ \i -> do
                         withImage i $ \img -> do
                              let (xs,ys) = unzip pts
                              xs' <- newArray $ map fromIntegral xs
                              ys' <- newArray $ map fromIntegral  ys
                              {#call wrapFillPolygon#} img 
                                   (fromIntegral $ length xs) xs' ys' 
                               (realToFrac c) 
                              free xs'
                              free ys'

rectOp c t (x,y) (x1,y1) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawRectangle#} img x y x1 y1 c t 

rectOpS c t (x,y) (w,h) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawRectangle#} img x y (x+w) (y+h) c t 

line color thickness start end i = 
    operate (lineOp color thickness start end ) i

rectangle color thickness a b i = 
    operate (rectOp color thickness a b ) i

fillPoly c pts i = operate (fillPolyOp c pts) i

drawLinesOp color thickness segments = 
    foldl (#>) nonOp 
     $ map (\(a,b) -> lineOp color thickness a b) segments

drawLines img color thickness segments = operateOn img
                    (drawLinesOp color thickness segments)

circle center r color s i = unsafeOperate (circleOp center r color s) i

floodfill (x,y) color low high floats = 
    unsafeOperate (fillOp (x,y) color low high floats) 

fillOp (x,y) color low high floats = 
    ImgOp $ \i -> do
      withImage i $ \img -> 
        ({#call wrapFloodFill#} img x y color low high 
                              (toCINT $ floats))
    where
     toCINT False = 0
     toCINT True  = 1
            

