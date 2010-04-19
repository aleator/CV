{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.Gabor where

{#import CV.Image #}
{#import CV.Filters #}
import CV.Image
import CV.Filters
import System.IO.Unsafe
import Foreign.C.Types
import Foreign.Ptr
import CV.Transforms

newtype GaborMask = GaborMask (CInt,CInt,CDouble,CDouble,CDouble,CDouble,CDouble) 

-- gaborFilterS 
--  (GaborMask (width,height,stdX,stdY,theta,phase,cycles)) image
--     = convolve2DI (width `div` 2,height `div` 2) kernel image
--  where
--   kernel = scale Cubic 0.5 
--              $ gaborImage 0 0 (GaborMask (2*width,2*height,stdX,stdY,theta,phase,cycles))
-- 

gaborImage (width,height,dx,dy,stdX,stdY,theta,phase,cycles) = 
    unsafePerformIO $ do
        img <- createImage32F (width,height) 1
        withGenImage img $ \i ->
            {#call renderGabor#} i width height dx dy stdX stdY theta phase cycles
        return img

gaborFiltering (GaborMask (width,height,stdX,stdY,theta,phase,cycles)) image = 
    unsafePerformIO $ 
        withClone image $ \img ->
        withGenImage img $ \clone ->
        withGenImage image $ \original ->
            {#call gaborFilter#} original clone width height 
                                 stdX stdY theta phase cycles

radialGaborFiltering (width,height,sigma,phase
                      ,center,cycles) image = 
    unsafePerformIO $ 
        withClone image $ \img ->
        withGenImage img $ \clone ->
        withGenImage image $ \original ->
            {#call radialGaborFilter#} original clone 
                width height 
                sigma phase center cycles

radialGaborImage (width,height,sigma,phase
                 ,center,cycles) = 
    unsafePerformIO $ do
        img <- createImage32F (width,height) 1
        withGenImage img $ \i ->
            {#call renderRadialGabor#} i width height sigma 
                                       phase center cycles
        return img

