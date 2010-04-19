{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.Filters(gaussian,gaussianOp,bilateral
              ,blurOp,blur,blurNS
              ,median,mkFilter,forWindows,center
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

-- higher level api for creating non-convolutional filters

-- Type for handling regions of image. Read this as "a function that
-- takes an image with ROI set, and returns a new value for the center pixel
type RegionOp = Image -> IO CDouble
-- Filter image using RegionOp. RegionOp is applied to
-- given size window, and its results are stored into a new
-- image. Window is specified using ROI
mkFilter :: (CInt,CInt) -> RegionOp -> ImageOperation 
mkFilter (w,h) rOp  
    | odd w && odd h = ImgOp $ \image ->  do
                        clone  <- cloneImage image
                        --result <- createImage resultSize imageDepth32F 1 
                        let (imageW,imageH) = getSize image
                        let wpad = w `div` 2
                        let hpad = h `div` 2
                        let resultSize = (imageW - (2*wpad),imageH - (2*hpad))
                        let windows = [(i,j) | i<-[0..fst resultSize-1]
                                             , j<-[0..snd resultSize-1]]
                        sequence_ [applyOp pos (wpad,hpad) clone image | pos <- windows]
                        resetROI clone
                        return ()

    | otherwise = error "Must have odd size window" -- have only odd size windows
    
        where
         (cw,ch) = (fromIntegral w,fromIntegral h)
         applyOp pos shift src dst = setROI pos (cw,ch) src 
                           >> (createPixel pos shift src dst)
         
         createPixel :: (CInt,CInt) -> (CInt,CInt) -> Image -> Image -> IO ()
         createPixel (x,y) (dx,dy) src dst = do
                                val <- rOp src
                                setPixel (x+dx,y+dy) val dst

forWindows (w,h) (imageW,imageH) op dst
    | odd w && odd h = do
                        let wpad = w `div` 2
                        let hpad = h `div` 2
                        let resultSize = (imageW - (2*wpad),imageH - (2*hpad))
                        let positions = [((i,j)) | i<-[0..fst resultSize-1]
                                                      , j<-[0..snd resultSize-1]]
                        sequence_ [createPixel pos (wpad,hpad) dst | pos <- positions]
                        return dst

    | otherwise = error "Must have odd size window"  -- have only odd size windows
    
        where
         (cw,ch) = (fromIntegral w,fromIntegral h)
         createPixel (x,y) (dx,dy) dst = do
                                val <- op ((x,y),(cw,ch))
                                setPixel (x+dx,y+dy) val dst

center ((x,y),(w,h)) = (x+(w `div` 2), y+(h `div` 2))
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
    {withGenImage* `Image'
    ,withGenImage* `Image'
    ,`Int',`Int',`Int',`Double',`Double'}
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

-- These work on 8 bit images only! Need a fix. TODO TODO!
bilateral colorS spaceS img = withClone img $ \clone ->
                   smooth' img clone  (fromEnum Bilateral)
                    colorS spaceS 0 0


median (w,h) img 
  | maskIsOk (w,h) = unsafePerformIO $ do
                    clone1 <- imageTo8Bit img
                    clone2 <- imageTo8Bit img
                    smooth' clone1 clone2  (fromEnum Median) w h 0 0
                    imageTo32F clone2
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

verticalAverage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- createImage32F (w,h) 1
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call vertical_average#} i sum 
                    return s

newtype IntegralImage = IntegralImage Image

getIISize (IntegralImage i) = getSize i

integralImage image = unsafePerformIO $ do 
                    let (w,h) = getSize image
                    s <- createImage64F (w+1,h+1) 1
                    withGenImage image $ \i -> do
                     withGenImage s $ \sum -> do
                      {#call cvIntegral#} i sum nullPtr nullPtr
                      return $ IntegralImage s


haar (IntegralImage image) (a',b',c',d') = unsafePerformIO $ do
                    let (w,h) = getSize image
                    let [a,b,c,d] = map fromIntegral [a',b',c',d']
                    r <- createImage32F (w,h) 1
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
