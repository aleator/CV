{-#LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleInstances#-}
#include "cvWrapLEO.h"
{-#OPTIONS_GHC -fwarn-unused-imports#-}

-- | This module is a collection of various image filters
module CV.Filters(
               gaussian,gaussianOp
              ,blurOp,blur,blurNS
              ,bilateral
              ,HasMedianFiltering,median
              ,susan,getCentralMoment,getAbsCentralMoment
              ,getMoment,secondMomentBinarize,secondMomentBinarizeOp
              ,secondMomentAdaptiveBinarize,secondMomentAdaptiveBinarizeOp
              ,selectiveAvg,convolve2D,convolve2DI,haar,haarAt
              ,IntegralImage(),integralImage,verticalAverage) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import CV.Bindings.ImgProc

import Utils.GeometryClass
import CV.Matrix (Matrix,withMatPtr)
import CV.ImageOp

import System.IO.Unsafe

--import C2HSTools
{#import CV.Image#}

-- Low level wrapper for Susan filtering:
--  IplImage* susanSmooth(IplImage *src, int w, int h
--                     ,double t, double sigma); 

-- | SUSAN adaptive smoothing filter, see <http://users.fmrib.ox.ac.uk/~steve/susan/susan/susan.html>
susan :: (Int, Int) -> Double -> Double
     -> Image GrayScale D32 -> Image GrayScale D32
susan (w,h) t sigma image = unsafePerformIO $ do
                            withGenImage image $ \img ->
                             creatingImage 
                                ({#call susanSmooth#} img (fromIntegral w) (fromIntegral h) 
                                                         (realToFrac t) (realToFrac sigma))
-- TODO: ADD checks above!

-- | A selective average filter is an edge preserving noise reduction filter.
--   It is a standard gaussian filter which ignores pixel values
--   that are more than a given threshold away from the filtered pixel value.
selectiveAvg :: (Int, Int) -> Double 
     -> Image GrayScale D32 -> Image GrayScale D32
selectiveAvg (w,h) t image = unsafePerformIO $ do
                              withGenImage image $ \img ->
                               creatingImage 
                                ({#call selectiveAvgFilter#} 
                                    img (realToFrac t) (fromIntegral w) (fromIntegral h))
-- TODO: ADD checks above!

getCentralMoment n (w,h) image = unsafePerformIO $ do
                            withGenImage image $ \img ->
                             creatingImage 
                                ({#call getNthCentralMoment#} img n w h)

getAbsCentralMoment n (w,h) image = unsafePerformIO $ do
                            withGenImage image $ \img ->
                             creatingImage 
                                ({#call getNthAbsCentralMoment#} img n w h)

getMoment n (w,h) image = unsafePerformIO $ do
                            withGenImage image $ \img ->
                             creatingImage 
                                ({#call getNthMoment#} img n w h)
-- TODO: ADD checks above!

secondMomentBinarizeOp t = ImgOp $ \image -> 
                            withGenImage image  (flip {#call smb#} $ t)
secondMomentBinarize t i = unsafeOperate (secondMomentBinarizeOp t) i

secondMomentAdaptiveBinarizeOp w h t = ImgOp $ \image -> 
                            withGenImage image  
                                (\i-> {#call smab#} i w h t)
secondMomentAdaptiveBinarize w h t i = unsafeOperate (secondMomentAdaptiveBinarizeOp w h t) i

#c
enum SmoothType {
    BlurNoScale =  CV_BLUR_NO_SCALE,
    Blur        =  CV_BLUR,
    Gaussian    =  CV_GAUSSIAN,
    Median      =  CV_MEDIAN ,
    Bilateral   =  CV_BILATERAL      
};
#endc
{#enum SmoothType {}#}

{#fun cvSmooth as smooth' 
    {withGenImage* `Image GrayScale D32'
    ,withGenImage* `Image GrayScale D32'
    ,`Int',`Int',`Int',`Float',`Float'}
    -> `()'#}


-- | Image operation which applies gaussian or unifarm smoothing with a given window size to the image.
gaussianOp,blurOp,blurNSOp :: (Int, Int) -> ImageOperation GrayScale D32
gaussianOp m =  withMask m $ \(w,h) img -> smooth' img img (fromEnum Gaussian) w h 0 0
blurOp m = withMask m $ \(w,h) img -> smooth' img img (fromEnum Blur) w h 0 0
blurNSOp m = withMask m $ \(w,h) img -> smooth' img img (fromEnum BlurNoScale) w h 0 0

-- | Create a new image by applying gaussian, or uniform smoothing.
gaussian,blur,blurNS :: (Int, Int) -> Image GrayScale D32 -> Image GrayScale D32
gaussian = unsafeOperate . gaussianOp
blur     = unsafeOperate . blurOp
blurNS   = unsafeOperate . blurNSOp

withMask (w,h) op 
    | maskIsOk (w,h) = ImgOp $ op (w,h)
    | otherwise = error "One of aperture dimensions is incorrect (should be >=1 and odd))"



-- | Apply bilateral filtering 
bilateral :: (Int,Int) -> (Int,Int) -> Image a D8 -> Image a D8
bilateral (w,h) (s1,s2) img = unsafePerformIO $ 
            withClone img $ \clone ->
             withGenImage img $ \cimg ->
              withGenImage clone $ \ccln -> do
                   {#call cvSmooth#} cimg ccln  (fromIntegral $ fromEnum Bilateral)
                        (fromIntegral w) (fromIntegral h) 
                        (realToFrac s1) (realToFrac s2) 


class HasMedianFiltering a where
    median :: (Int,Int) -> a -> a

instance HasMedianFiltering (Image GrayScale D8) where
    median = median'

instance HasMedianFiltering (Image GrayScale D32) where
    median a = unsafeImageTo32F . median' a . unsafeImageTo8Bit

instance HasMedianFiltering (Image RGB D8) where
    median = median'

-- | Perform median filtering on an eight bit image.
median' :: (Int,Int) -> Image c D8 -> Image c D8
median' (w,h) img 
  | maskIsOk (w,h) = unsafePerformIO $ do
                    clone2 <- cloneImage img
                    withGenImage img $ \c1 -> 
                     withGenImage clone2 $ \c2 -> 
                        {#call cvSmooth#} c1 c2  (fromIntegral $ fromEnum Median) 
                                                 (fromIntegral w) (fromIntegral h) 0 0
                    return clone2
  | otherwise = error "One of aperture dimensions is incorrect (should be >=1 and odd))"

maskIsOk (w,h) = odd w && odd h && w >0 && h>0


-- General 2D comvolutions
-- Convolve image with specified kernel stored in flat list.
-- Kernel must have dimensions (w,h) and specified anchor point
-- (x,y) within (0,0) and (w,h)
convolve2D :: (Point2D anchor, ELP anchor ~ Int) => 
              Matrix D32 -> anchor -> Image GrayScale D32 -> Image GrayScale D32
convolve2D kernel anchor image = unsafePerformIO $ 
                                      let result = emptyCopy image
                                      in withGenImage image $ \c_img->
                                         withGenImage result $ \c_res->
                                         withMatPtr kernel $ \c_mat ->
                                         with (convertPt anchor) $ \c_pt ->
                                         c'wrapFilter2 c_img c_res c_mat c_pt
                                         >> return result
                                    
convolve2DI (x,y) kernel image = unsafePerformIO $ 
                                      withImage image $ \img->
                                      withImage kernel $ \k ->
                                      creatingImage $
                                       {#call wrapFilter2DImg#} 
                                        img k x y

-- | Replace pixel values by the average of the row. 
verticalAverage :: Image GrayScale D32 -> Image GrayScale D32
verticalAverage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- create (w,h) 
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call vertical_average#} i sum 
                    return s

-- | A type for storing integral images. Integral image stores for every pixel the sum of pixels
--   above and left of it. Such images are used for significantly accelerating the calculation of
--   area averages. 
newtype IntegralImage = IntegralImage (Image GrayScale D64)
instance Sized IntegralImage where
    type Size IntegralImage = (Int,Int)
    getSize (IntegralImage i) = getSize i

instance GetPixel IntegralImage where
    type P IntegralImage = Double
    getPixel = getPixel

-- | Calculate the integral image from the given image.
integralImage :: Image GrayScale D32 -> IntegralImage
integralImage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- create (w+1,h+1)
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call cvIntegral#} i sum nullPtr nullPtr
                      return $ IntegralImage s


-- |Filter the image with box shaped averaging mask.
haar :: IntegralImage -> (Int,Int,Int,Int) -> Image GrayScale D32
haar (IntegralImage image) (a',b',c',d') = unsafePerformIO $ do
                    let (w,h) = getSize image
                    let [a,b,c,d] = map fromIntegral [a',b',c',d']
                    r <- create (w,h)
                    withImage image $ \sum ->
                     withImage r $ \res -> do
                            {#call haarFilter#} sum 
                                (min a c) 
                                (max b d)
                                (max a c)
                                (min b d) 
                                res
                            return r

-- | Get an average of a given region.
haarAt  :: IntegralImage -> (Int,Int,Int,Int) -> Double

haarAt (IntegralImage ii) (a,b,w,h) = realToFrac $ unsafePerformIO $ withImage ii $ \i -> 
                                        {#call haar_at#} i (f a) (f b) (f w) (f h)
                                    where f = fromIntegral 
