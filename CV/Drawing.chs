{-#LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, 
            ViewPatterns, FlexibleContexts #-}
#include "cvWrapLEO.h"
-- | Module for exposing opencv drawing functions. These are meant for quick and dirty marking
--   and not for anything presentable. For any real drawing
--   you should figure out how to use cairo or related package, such as diagrams. They are
--   way better.
--
--   Consult the "CV.ImageOp" module for functions to apply the operations in this module to images.

module CV.Drawing(
                -- * Drawable class
                 ShapeStyle(Filled,Stroked)
                ,Drawable(..)
                -- * Extra drawing operations
                ,drawLinesOp
                ,drawBox2Dop 
                -- * Floodfill operations
                ,fillOp
                ,floodfill
                -- * Shorthand for drawing single shapes
                ,circle
                ,drawLines
                ,rectangle
                ,fillPoly) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Control.Monad(when)
import CV.Bindings.Types
import CV.Bindings.Drawing
import Utils.Point

{#import CV.Image#}

import CV.ImageOp
import Utils.GeometryClass
import Utils.Rectangle

-- | Is the shape filled or just a boundary?
data ShapeStyle = Filled | Stroked Int
    deriving(Eq,Show)

styleToCV Filled = -1
styleToCV (Stroked w) = fromIntegral w

-- TODO: The instances in here could be significantly smaller..
-- |Typeclass for images that support elementary drawing operations. 
class Drawable a b where
    -- | Type of the pixel, i.e. Float for a grayscale image and 3-tuple for RGB image.
    type Color a b :: * 
    -- | Put text of certain color to given coordinates. Good size seems to be around 0.5-1.5.
    putTextOp :: (Color a b) -> Float -> String -> (Int,Int) -> ImageOperation a b
    -- | Draw a line between two points.
    lineOp :: (Color a b)  -> Int -> (Int,Int) -> (Int,Int) -> ImageOperation a b
    -- | Draw a Circle
    circleOp :: (Color a b) -> (Int,Int) -> Int -> ShapeStyle -> ImageOperation a b
    -- | Draw a Rectangle by supplying two corners
    rectOp   :: (BoundingBox bb, Integral (ELBB bb)) => (Color a b) -> Int -> bb -> ImageOperation a b
    -- | Draw a filled polygon
    fillPolyOp :: (Color a b) -> [(Int,Int)] -> ImageOperation a b
    ellipseBoxOp :: (Color a b) -> C'CvBox2D -> Int -> Int -> ImageOperation a b

primRectOp (r,g,b) t (bounds -> Rectangle x y w h) = ImgOp $ \i -> do
                         withGenImage i $ \img -> 
                              {#call wrapDrawRectangle#} img (fromIntegral x)
                               (fromIntegral y) (fromIntegral $ x+w) (fromIntegral $ y+h)
                               (realToFrac r) (realToFrac g)(realToFrac b)(fromIntegral t)

-- | Primitive form of ellipse box. Not typesafe, not for end user.
primEllipseBox :: (D32,D32,D32,D32) -> C'CvBox2D -> Int -> Int -> ImageOperation c d

primEllipseBox (a,b,c,e) box thickness shift = 
            ImgOp          $ \i -> 
            withGenImage i $ \c_img -> 
            with box       $ \c_box ->
            with (C'CvScalar (rtf a) (rtf b) (rtf c) (rtf 0)) $ \c_color -> 
             c'wrapEllipseBox c_img c_box c_color (fromIntegral thickness) 8 
                              (fromIntegral shift)

rtf = realToFrac

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
    
    ellipseBoxOp (r,g,b) = primEllipseBox (r,g,b,0) 


    circleOp (red,g,b) (x,y) r s = ImgOp $ \i -> do
                        when (r>0) $ withGenImage i $ \img -> 
                              ({#call wrapDrawCircle#} img (fromIntegral x) (fromIntegral y) 
                                                           (fromIntegral r) (realToFrac red) 
                                                           (realToFrac g) (realToFrac b)
                                                           $ styleToCV s)

    rectOp c = primRectOp c

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
    ellipseBoxOp c  = primEllipseBox (c,c,c,0) 

    rectOp c = primRectOp (c,c,c)
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


-- | Flood fill a region of the image
fillOp :: (Int,Int) -> D32 -> D32 -> D32 -> Bool -> ImageOperation GrayScale D32
fillOp (x,y) color low high floats = 
    ImgOp $ \i -> do
      withImage i $ \img -> 
        ({#call wrapFloodFill#} img (fromIntegral x) (fromIntegral y)
            (realToFrac color) (realToFrac low) (realToFrac high) (toCINT $ floats))
    where
     toCINT False = 0
     toCINT True  = 1

-- | Apply rectOp to an image
rectangle :: (BoundingBox bb, Integral (ELBB bb), Drawable c d) 
             => Color c d -> Int -> bb -> Image c d
             -> IO (Image c d)
rectangle color thickness rect i = 
    operate (rectOp color thickness rect) i

-- | Apply fillPolyOp to an image
fillPoly :: Drawable c d => Color c d -> [(Int, Int)] -> Image c d -> IO (Image c d)
fillPoly c pts i = operate (fillPolyOp c pts) i

-- | Draw a polyline
drawLinesOp :: Drawable c d => Color c d -> Int -> [((Int, Int), (Int, Int))] -> CV.ImageOp.ImageOperation c d
drawLinesOp color thickness segments = 
    foldl (#>) nonOp 
     $ map (\(a,b) -> lineOp color thickness a b) segments

-- | Apply drawLinesOp to an image
drawLines :: Drawable c d => Image c d -> Color c d -> Int -> [((Int, Int), (Int, Int))]
                                -> IO (Image c d)
drawLines img color thickness segments = operateOn img
                    (drawLinesOp color thickness segments)

-- | Draw C'CvBox2D
drawBox2Dop :: Drawable c d => Color c d -> C'CvBox2D -> ImageOperation c d
drawBox2Dop color (C'CvBox2D (C'CvPoint2D32f (realToFrac -> x) (realToFrac ->y))
                             (C'CvSize2D32f  (realToFrac -> w) (realToFrac ->h)) 
                             (degToRad -> θ)) 
    = drawLinesOp color 1 (zip corners $ tail (cycle corners)) 
  where
    rot (x,y) = (x * sin (-θ) - y * cos (-θ)
                ,x * cos (-θ) + y * sin (-θ))
    corners = map (both round . (+ (x,y)) . rot) 
              [( 0.5*h,  0.5*w)
              ,(-0.5*h,  0.5*w)
              ,(-0.5*h, -0.5*w)
              ,( 0.5*h, -0.5*w) ]
    both f (a,b) = (f a, f b)

degToRad deg = deg/180*pi


-- | Apply circleOp to an image
circle :: Drawable c d => (Int, Int) -> Int -> Color c d -> ShapeStyle -> Image c d -> Image c d
circle center r color s i = unsafeOperate (circleOp color center r s) i

-- | Apply fillOp to an image
floodfill :: (Int, Int) -> D32 -> D32 -> D32 -> Bool -> Image GrayScale D32 -> Image GrayScale D32
floodfill (x,y) color low high floats = 
    unsafeOperate (fillOp (x,y) color low high floats) 

            

