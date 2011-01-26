{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.Edges where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.ImageOp

import CV.Image 
{#import CV.Image#}

import C2HSTools

sobelOp :: (Int,Int) -> Int -> ImageOperation GrayScale D32
sobelOp (dx,dy) aperture 
    | dx >=0 && dx <3
    && aperture `elem` [-1,1,3,5,7]
    && not ((aperture == -1) && (dx>1 || dy>1))
    && dy >=0 && dy<3 = ImgOp $ \i -> withGenImage i $ \image ->
                                      ({#call cvSobel#} image image cdx cdy cap)

   | otherwise = error "Invalid aperture" 
      where [cdx,cdy,cap] = map fromIntegral [dx,dy,aperture]

sobel dd ap im = unsafeOperate (sobelOp dd ap) im


laplaceOp :: Int -> ImageOperation GrayScale D32
laplaceOp s = ImgOp $ \img ->  withGenImage img $ \image -> 
                    if s `elem` [1,3,5,7]
                       then ({#call cvLaplace #} image image (fromIntegral s)) 
                       else error "Laplace aperture must be 1, 3, 5 or 7"
laplace s i = unsafeOperate (laplaceOp s) i

-- TODO: Add tests below!
canny :: Int -> Int -> Int -> Image GrayScale D8 -> Image GrayScale D8
canny t1 t2 aperture src = unsafePerformIO $ do
                           withClone src $ \clone -> 
                            withGenImage src $ \si ->
                             withGenImage clone $ \ci -> do
                               {#call cvCanny#} si ci (fromIntegral t1) 
                                                      (fromIntegral t2) 
                                                      (fromIntegral aperture)
                               return clone
                               
                            
                             

susan :: (Int,Int) -> D32 -> Image GrayScale D32 -> Image GrayScale D8
susan (w,h) t image = unsafePerformIO $ do
                    withGenImage image $ \img ->
                     creatingImage
                      ({#call susanEdge#} img (fromIntegral w) (fromIntegral h) (realToFrac t))
