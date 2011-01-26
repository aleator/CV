{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.Filters(gaussian,gaussianOp,bilateral
              ,blurOp,blur,blurNS
              ,median
              ,susan,getCentralMoment,getAbsCentralMoment
              ,getMoment,secondMomentBinarize,secondMomentBinarizeOp
              ,secondMomentAdaptiveBinarize,secondMomentAdaptiveBinarizeOp
              ,selectiveAvg,convolve2D,convolve2DI,haar,haarAt
              ,IntegralImage,getIISize,integralImage,verticalAverage) where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.Image 
import CV.ImageOp
import Debug.Trace

import C2HSTools
{#import CV.Image#}

-- Low level wrapper for Susan filtering:
--  IplImage* susanSmooth(IplImage *src, int w, int h
--                     ,double t, double sigma); 

susan (w,h) t sigma image = unsafePerformIO $ do
                            withGenImage image $ \img ->
                             creatingImage 
                                ({#call susanSmooth#} img w h t sigma)
-- TODO: ADD checks above!
selectiveAvg (w,h) t image = unsafePerformIO $ do
                              withGenImage image $ \img ->
                               creatingImage 
                                ({#call selectiveAvgFilter#} 
                                    img t w h)
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

-- Low level wrapper for opencv
data SmoothType = BlurNoScale | Blur 
                | Gaussian | Median 
                | Bilateral
                deriving(Enum)

{#fun cvSmooth as smooth' 
    {withGenImage* `Image GrayScale D32'
    ,withGenImage* `Image GrayScale D32'
    ,`Int',`Int',`Int',`Float',`Float'}
    -> `()'#}

gaussianOp (w,h) 
    | maskIsOk (w,h) = ImgOp $ \img -> 
                               smooth' img img (fromEnum Gaussian) w h 0 0
    | otherwise = error "One of aperture dimensions is incorrect (should be >=1 and odd))"

gaussian = unsafeOperate.gaussianOp

blurOp (w,h) 
    | maskIsOk (w,h) = ImgOp $ \img -> 
                               smooth' img img (fromEnum Blur) w h 0 0
    | otherwise = error "One of aperture dimensions is incorrect (should be >=1 and odd))"

blurNSOp (w,h) 
    | maskIsOk (w,h) = ImgOp $ \img -> 
                               smooth' img img (fromEnum BlurNoScale) w h 0 0
    | otherwise = error "One of aperture dimensions is incorrect (should be >=1 and odd))"

blur size image = let r = unsafeOperate (blurOp size) image
                in  r
blurNS size image = let r = unsafeOperate (blurNSOp size) image
                in  r

-- | TODO: This doesn't give a reasonable result. Investigate
bilateral :: Int -> Int -> Image GrayScale D8 -> Image GrayScale D8
bilateral colorS spaceS img = unsafePerformIO $ 
            withClone img $ \clone ->
             withGenImage img $ \cimg ->
              withGenImage clone $ \ccln -> do
                   {#call cvSmooth#} cimg ccln  (fromIntegral $ fromEnum Bilateral)
                        (fromIntegral colorS) (fromIntegral spaceS) 0 0


median :: (Int,Int) -> Image GrayScale D8 -> Image GrayScale D8
median (w,h) img 
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
convolve2D (w,h) (x,y) kernel image = unsafePerformIO $ 
                                      withImage image $ \img->
                                      withArray kernel $ \k ->
                                      creatingImage $
                                       {#call wrapFilter2D#} 
                                        img x y w h k

convolve2DI (x,y) kernel image = unsafePerformIO $ 
                                      withImage image $ \img->
                                      withImage kernel $ \k ->
                                      creatingImage $
                                       {#call wrapFilter2DImg#} 
                                        img k x y

verticalAverage :: Image GrayScale D32 -> Image GrayScale D32
verticalAverage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- create (w,h) 
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call vertical_average#} i sum 
                    return s

newtype IntegralImage = IntegralImage (Image GrayScale D64)

getIISize (IntegralImage i) = getSize i

integralImage :: Image GrayScale D32 -> IntegralImage
integralImage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- create (w+1,h+1)
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call cvIntegral#} i sum nullPtr nullPtr
                      return $ IntegralImage s


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

haarAt (IntegralImage ii) (a,b,w,h) = unsafePerformIO $ withImage ii $ \i -> 
                                        {#call haar_at#} i a b w h 
