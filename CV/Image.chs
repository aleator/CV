{-#LANGUAGE ForeignFunctionInterface, ViewPatterns, TypeFamilies #-}
#include "cvWrapLeo.h"
module CV.Image where

import System.Posix.Files
import System.Mem

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Parallel.Strategies
import Control.DeepSeq

-- import C2HSTools

import Data.Maybe(catMaybes)
import Data.List(genericLength)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe


data GrayScale
data RGB
data LAB

newtype NewImage channels depth = S Image

--data family Pixel channel depth
--data instance Pixel GrayScale Double = Double
--data instance Pixel RGB Double = (Double,Double,Double)

unS (S i ) = i -- Unsafe and ugly


{#pointer *IplImage as Image foreign newtype#}

foreign import ccall "& wrapReleaseImage" releaseImage :: FinalizerPtr Image

instance NFData Image where
    rnf a@(Image fptr) = (unsafeForeignPtrToPtr) fptr `seq` a `seq` ()-- This might also need peek?


creatingImage fun = do
              iptr <- fun
--              {#call incrImageC#} -- Uncomment this line to get statistics of number of images allocated by ghc
              fptr <- newForeignPtr releaseImage iptr
              return.Image $ fptr

unImage (Image fptr) = fptr
composeMultichannelImage :: Maybe Image -> Maybe Image -> Maybe Image -> Maybe Image -> Image 
composeMultichannelImage c1 c2 c3 c4 = unsafePerformIO $ do
        res <- createImage32F (size) 4 -- TODO: Check channel count
        withMaybe c1 $ \cc1 -> 
         withMaybe c2 $ \cc2 -> 
          withMaybe c3 $ \cc3 -> 
           withMaybe c4 $ \cc4 -> 
            withGenImage res $ \cres -> {#call cvMerge#} cc1 cc2 cc3 cc4 cres
        return res
    where
        withMaybe (Just i) op = withGenImage i op
        withMaybe (Nothing) op = op nullPtr
        size = getSize . head . catMaybes $ [c1,c2,c3,c4]

-- Load Image as grayscale image.

loadImage n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingImage ({#call cvLoadImage #} name (0))
                              bw <- imageTo32F i
                              return $ Just bw

loadImageNew :: FilePath -> IO (Maybe (NewImage GrayScale Double))
loadImageNew n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingImage ({#call cvLoadImage #} name (0))
                              bw <- imageTo32F i
                              return . Just . S $ bw

loadColorImageNew :: FilePath -> IO (Maybe (NewImage RGB Double))
loadColorImageNew n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingImage ({#call cvLoadImage #} name 1)
                              bw <- imageTo32F i
                              return . Just . S  $ bw

getSizeNew :: (Integral a, Integral b) => NewImage c d -> (a,b)
getSizeNew image = unsafePerformIO $ withImage (unS image) $ \i -> do
                 w <- {#call getImageWidth#} i
                 h <- {#call getImageHeight#} i
                 return (fromIntegral w,fromIntegral h)

loadColorImage n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingImage ({#call cvLoadImage #} name 1)
                              bw <- imageTo32F i
                              return $ Just bw

cvRGBtoGRAY = 7 :: CInt-- NOTE: This will break.
cvRGBtoLAB = 45 :: CInt-- NOTE: This will break.

convertToGrayScale img = unsafePerformIO $ creatingImage $ do
    res <- {#call wrapCreateImage32F#} w h 1
    withImage img $ \cimg -> 
        {#call cvCvtColor#} (castPtr cimg) (castPtr res) cvRGBtoGRAY
    return res
 where    
    (w,h) = getSize img

rgbToLab :: NewImage RGB Double -> NewImage LAB Double
rgbToLab = S . convertTo cvRGBtoLAB 3 . unS 

rgbToGray :: NewImage RGB Double -> NewImage GrayScale Double
rgbToGray = S . convertToGrayScale . unS

getRegionNew :: (Integral a) => (a,a) -> (a,a) -> NewImage c d -> NewImage c d
getRegionNew a b
    = S . getRegion a b . unS

class GetPixel a where
    type P a :: *
    getPixelNew   :: (Integral i) => (i,i) -> a -> P a

instance GetPixel (NewImage GrayScale Double) where
    type P (NewImage GrayScale Double) = Double 
    getPixelNew (fromIntegral -> x, fromIntegral -> y) image = realToFrac $ unsafePerformIO $ withGenImage (unS image) $ \img ->
                              {#call wrapGet32F2D#} img y x

instance  GetPixel (NewImage RGB Double) where
    type P (NewImage RGB Double) = (Double,Double,Double) 
    getPixelNew (fromIntegral -> x, fromIntegral -> y) image 
        = unsafePerformIO $ do 
                     withGenImage (unS image) $ \img -> do
                              r <- {#call wrapGet32F2DC#} img y x 0
                              g <- {#call wrapGet32F2DC#} img y x 1
                              b <- {#call wrapGet32F2DC#} img y x 2
                              return (realToFrac r,realToFrac g, realToFrac b)


convertTo code channels img = unsafePerformIO $ creatingImage $ do
    res <- {#call wrapCreateImage32F#} w h channels
    withImage img $ \cimg -> 
        {#call cvCvtColor#} (castPtr cimg) (castPtr res) code
    return res
 where    
    (w,h) = getSize img

createImage32F (w,h) nChannels = do
    creatingImage $ {#call wrapCreateImage32F#} w h nChannels

createImage64F (w,h) nChannels = do
    creatingImage $ {#call wrapCreateImage64F#} w h nChannels

createImage8U (w,h) nChannels = do
    creatingImage $ {#call wrapCreateImage8U#} w h nChannels

image32F size channels = unsafePerformIO $ createImage32F size channels
image8U size channels = unsafePerformIO $ createImage8U size channels

emptyCopy img = image32F (getSize img) 1

saveImage filename image = do
                           fpi <- imageTo8Bit image
                           withCString  filename $ \name  -> 
                            withGenImage fpi    $ \cvArr ->
							 alloca (\defs -> poke defs 0 >> {#call cvSaveImage #} name cvArr defs >> return ())

getSize image = unsafePerformIO $ withImage image $ \i -> do
                 w <- {#call getImageWidth#} i
                 h <- {#call getImageHeight#} i
                 return (fromIntegral w,fromIntegral h)

getArea = uncurry (*).getSize

getRegion :: (Integral a) => (a, a) -> (a,a) -> Image -> Image
getRegion (fromIntegral -> x,fromIntegral -> y) (fromIntegral -> w,fromIntegral -> h) image 
    | x+w <= width && y+h <= height = getRegion' (x,y) (w,h) image
    | otherwise                   = error $ "Region outside image:"
                                            ++ show (getSize image) ++
                                            "/"++show (x+w,y+h)
 where
  (width,height) = getSize image
    
getRegion' (x,y) (w,h) image = unsafePerformIO $
                               withImage image $ \i ->
                                 creatingImage ({#call getSubImage#} 
                                                i x y w h)


-- | Tile images by overlapping them on a black canvas.
tileImages image1 image2 (x,y) = unsafePerformIO $
                               withImage image1 $ \i1 ->
                                withImage image2 $ \i2 ->
                                 creatingImage ({#call simpleMergeImages#} 
                                                i1 i2 x y)
-- | Blit image2 onto image1. 
--blit image1 image2 (x,y) =
--                          withImage image1 $ \i1 ->
--                           withImage image2 $ \i2 ->
--                            ({#call plainBlit#} i1 i2 x y)
-- TODO: Remove the above
blitFix = blit
blit image1 image2 (x,y) 
    | badSizes  = error $ "Bad blit sizes: " ++ show [(w1,h1),(w2,h2)]++"<-"++show (x,y) 
    | otherwise = withImage image1 $ \i1 ->
                   withImage image2 $ \i2 ->
                    ({#call plainBlit#} i1 i2 y x)
    where 
     ((w1,h1),(w2,h2)) = (getSize image1,getSize image2)
     badSizes = x+w2>w1 || y+h2>h1 || x<0 || y<0
subPixelBlit
  :: Image -> Image -> (CDouble, CDouble) -> IO ()

subPixelBlit image1 image2 (x,y) 
    | badSizes  = error $ "Bad blit sizes: " ++ show [(w1,h1),(w2,h2)]++"<-"++show (x,y) 
    | otherwise = withImage image1 $ \i1 ->
                   withImage image2 $ \i2 ->
                    ({#call subpixel_blit#} i1 i2 y x)
    where 
     ((w1,h1),(w2,h2)) = (getSize image1,getSize image2)
     badSizes = ceiling x+w2>w1 || ceiling y+h2>h1 || x<0 || y<0

safeBlit i1 i2 (x,y) = unsafePerformIO $ do
                  res <- cloneImage i1-- createImage32F (getSize i1) 1
                  blit res i2 (x,y)
                  return res

-- | Blit image2 onto image1. 
--   This uses an alpha channel bitmap for determining the regions where the image should be "blended" with 
--   the base image.
blendBlit image1 image1Alpha image2 image2Alpha (x,y) = 
                               withImage image1 $ \i1 ->
                                withImage image1Alpha $ \i1a ->
                                 withImage image2Alpha $ \i2a ->
                                  withImage image2 $ \i2 ->
                                   ({#call alphaBlit#} i1 i1a i2 i2a x y)


cloneImage img = withGenImage img $ \image ->  
                    creatingImage ({#call cvCloneImage #} image)

withClone img fun = do 
                result <- cloneImage img
                fun result
                return result

imageTo32F img = withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure32F #} image)
imageTo8Bit img = withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure8U #} image)
-- Ok. this is just the example why I need image types
withUniPtr with x fun = with x $ \y -> 
                    fun (castPtr y)

withGenImage = withUniPtr withImage

-- Manipulating regions of interest:
setROI (x,y) (w,h) image = withImage image $ \i -> 
                            {#call wrapSetImageROI#} i x y w h
resetROI image = withImage image $ \i ->
                  {#call cvResetImageROI#} i

setCOI chnl image = withImage image $ \i -> 
                            {#call cvSetImageCOI#} i (fromIntegral chnl)
resetCOI image = withImage image $ \i ->
                  {#call cvSetImageCOI#} i 0

getChannel no image = unsafePerformIO $ creatingImage $ do
    let (w,h) = getSize image
    setCOI no image
    cres <- {#call wrapCreateImage32F#} w h 1
    withGenImage image $ \cimage ->
      {#call cvCopy#} cimage (castPtr cres) (nullPtr)
    resetCOI image
    return cres

withIOROI pos size image op = do
            setROI pos size image
            x <- op
            resetROI image
            return x

withROI pos size image op = unsafePerformIO $ do
                        setROI pos size image
                        let x = op image
                        resetROI image
                        return x

-- Manipulating image pixels
setPixel :: (CInt,CInt) -> CDouble -> Image -> IO ()
setPixel (x,y) v image = withGenImage image $ \img ->
                          {#call wrapSet32F2D#} img y x v

getPixel :: (CInt,CInt) -> Image -> CDouble
getPixel (x,y) image = unsafePerformIO $ withGenImage image $ \img ->
                          {#call wrapGet32F2D#} img y x

getAllPixels image =  [getPixel (i,j) image 
                      | i <- [0..width-1 ]
                      , j <- [0..height-1]]                          
                    where
                     (width,height) = getSize image

getAllPixelsRowMajor image =  [getPixel (i,j) image 
                              | j <- [0..height-1]
                              , i <- [0..width-1]
                              ]                          
                    where
                     (width,height) = getSize image

-- |Create a montage form given images (u,v) determines the layout and space the spacing
--  between images. Images are assumed to be the same size (determined by the first image)
montage :: (Int,Int) -> Int -> [Image] -> Image
montage (u',v') space' imgs = resultPic
    where
     space = fromIntegral space'
     (u,v) = (fromIntegral u', fromIntegral v')
     (rw,rh) = (u*xstep,v*ystep) 
     (w,h) = getSize (head imgs)
     (xstep,ystep) = (fromIntegral space + w,fromIntegral space + h)
     edge = space`div`2
     resultPic = unsafePerformIO $ do
                    r <- createImage32F (rw,rh) 1
                    sequence_ [blit r i (edge +  x*xstep, edge + y*ystep) | y <- [0..v-1] , x <- [0..u-1] | i <- imgs ]
                    return r

