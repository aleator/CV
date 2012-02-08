{-#LANGUAGE ForeignFunctionInterface, DataKinds, PolyKinds, TypeFamilies #-}

#include "cvWrapLEO.h"
-- | This module is a collection of simple edge detectors.
module CV.Edges (
                -- * Common edge detectors
                 sobelOp,sobel
                ,laplaceOp,laplace,canny,susan
                -- * Various aperture sizes
                -- | For added safety  we define the possible 
                --   apertures as constants, since the filters accept only
                --   specific mask sizes.
                ,sScharr,s1,s3,s5,s7
                ,l1,l3,l5,l7
                ) where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.ImageOp

import CV.Image 
{#import CV.Image#}

import C2HSTools hiding (unsafePerformIO)
import System.IO.Unsafe

-- | Perform Sobel filtering on image. First argument gives order of horizontal and vertical
--   derivative estimates and second one is the aperture. This function can also calculate
--   Scharr filter with aperture specification of sScharr

data Orders = O0_1 
            | O0_2
            | O1_0
            | O1_1
            | O1_2
            | O2_0
            | O2_1
            | O2_2
          deriving (Eq,Ord,Show)
              
type family Larger a b :: Bool
type instance Larger D8 D32  = True
type instance Larger D8 D64  = True
type instance Larger D32 D32 = True
type instance Larger D32 D64 = True


sobelOp :: (Int,Int) -> SobelAperture -> ImageOperation GrayScale D32
sobelOp (dx,dy) (Sb aperture)
    | dx >=0 && dx <3
    && not ((aperture == -1) && (dx>1 || dy>1))
    && dy >=0 && dy<3 = ImgOp $ \i -> withGenImage i $ \image ->
                                      ({#call cvSobel#} image image cdx cdy cap)

   | otherwise = error "Invalid aperture" 
      where [cdx,cdy,cap] = map fromIntegral [dx,dy,aperture]

sobel dd ap im = unsafeOperate (sobelOp dd ap) im

-- | Aperture sizes for sobel operator
newtype SobelAperture = Sb Int
-- | Use Scharr mask instead
sScharr = Sb (-1) 
s1 = Sb 1
s3 = Sb 3
s5 = Sb 5
s7 = Sb 7


-- | Aperture sizes for laplacian operator
newtype LaplacianAperture = L Int
l1 = L 1
l3 = L 3
l5 = L 5
l7 = L 7

-- |Perform laplacian filtering of given aperture to image
laplaceOp :: LaplacianAperture -> ImageOperation GrayScale D32
laplaceOp (L s) = ImgOp $ \img ->  withGenImage img $ \image -> 
                        ({#call cvLaplace #} image image (fromIntegral s)) 

laplace s i = unsafeOperate (laplaceOp s) i

-- |Perform canny thresholding using two threshold values and given aperture
--  Works only on 8-bit images
canny :: Int -> Int -> Int -> Image GrayScale D8 -> Image GrayScale D8
canny t1 t2 aperture src = unsafePerformIO $ do
                           withCloneValue src $ \clone -> 
                            withGenImage src $ \si ->
                             withGenImage clone $ \ci -> do
                               {#call cvCanny#} si ci (fromIntegral t1) 
                                                      (fromIntegral t2) 
                                                      (fromIntegral aperture)
                               return clone
                               
                            
                             
-- | SUSAN edge detection filter, see <http://users.fmrib.ox.ac.uk/~steve/susan/susan/susan.html>
susan :: (Int,Int) -> D32 -> Image GrayScale D32 -> Image GrayScale D8
susan (w,h) t image = unsafePerformIO $ do
                    withGenImage image $ \img ->
                     creatingImage
                      ({#call susanEdge#} img (fromIntegral w) (fromIntegral h) (realToFrac t))
-- TODO: Should return a binary image
