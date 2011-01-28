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
import Data.Word


-- Colorspaces
data GrayScale
data RGB
data RGBA
data LAB

-- Bit Depths
type D8  = Word8
type D32 = Float
type D64 = Double

newtype Image channels depth = S BareImage

unS (S i) = i -- Unsafe and ugly

withImage :: Image c d -> (Ptr BareImage ->IO a) -> IO a
withImage (S i) op = withBareImage i op
--withGenNewImage (S i) op = withGenImage i op 

-- Ok. this is just the example why I need image types
withUniPtr with x fun = with x $ \y -> 
                    fun (castPtr y)

withGenImage = withUniPtr withImage
withGenBareImage = withUniPtr withBareImage

{#pointer *IplImage as BareImage foreign newtype#}

foreign import ccall "& wrapReleaseImage" releaseImage :: FinalizerPtr BareImage

instance NFData (Image a b) where
    rnf a@(S (BareImage fptr)) = (unsafeForeignPtrToPtr) fptr `seq` a `seq` ()-- This might also need peek?


creatingImage fun = do
              iptr <- fun
--              {#call incrImageC#} -- Uncomment this line to get statistics of number of images allocated by ghc
              fptr <- newForeignPtr releaseImage iptr
              return . S . BareImage $ fptr

creatingBareImage fun = do
              iptr <- fun
--              {#call incrImageC#} -- Uncomment this line to get statistics of number of images allocated by ghc
              fptr <- newForeignPtr releaseImage iptr
              return . BareImage $ fptr

unImage (S (BareImage fptr)) = fptr

data Tag tp;
rgb = undefined :: Tag RGB
lab = undefined :: Tag LAB


composeMultichannelImage :: (CreateImage (Image tp a)) => Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Tag tp -> Image tp a
composeMultichannelImage (c1) 
                         (c2)
                         (c3)
                         (c4)
                         totag
    = unsafePerformIO $ do
        res <- create (size) -- TODO: Check channel count -- This is NOT correct
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

--loadImage n = do
--              exists <- fileExist n
--              if not exists then return Nothing
--                            else do
--                              i <- withCString n $ \name -> 
--                                     creatingImage ({#call cvLoadImage #} name (0))
--                              bw <- imageTo32F i
--                              return $ Just bw

loadImage :: FilePath -> IO (Maybe (Image GrayScale D32))
loadImage n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingBareImage ({#call cvLoadImage #} name (0))
                              bw <- imageTo32F i
                              return . Just . S $ bw

loadColorImage :: FilePath -> IO (Maybe (Image RGB D32))
loadColorImage n = do
              exists <- fileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name -> 
                                     creatingBareImage ({#call cvLoadImage #} name 1)
                              bw <- imageTo32F i
                              return . Just . S  $ bw

class IntSized a where
    getSize :: a -> (Int,Int)

instance IntSized BareImage where
   -- getSize :: (Integral a, Integral b) => Image c d -> (a,b)
    getSize image = unsafePerformIO $ withBareImage image $ \i -> do
                 w <- {#call getImageWidth#} i
                 h <- {#call getImageHeight#} i
                 return (fromIntegral w,fromIntegral h)

instance IntSized (Image c d) where
    getSize = getSize . unS

--loadColorImage n = do
--              exists <- fileExist n
--              if not exists then return Nothing
--                            else do
--                              i <- withCString n $ \name -> 
--                                     creatingImage ({#call cvLoadImage #} name 1)
--                              bw <- imageTo32F i
--                              return $ Just bw

cvRGBtoGRAY = 7 :: CInt-- NOTE: This will break.
cvRGBtoLAB = 45 :: CInt-- NOTE: This will break.


rgbToLab :: Image RGB Double -> Image LAB Double
rgbToLab = S . convertTo cvRGBtoLAB 3 . unS

rgbToGray :: Image RGB Double -> Image GrayScale Double
rgbToGray = S . convertTo cvRGBtoGRAY 1 . unS


class GetPixel a where
    type P a :: *
    getPixel   :: (Integral i) => (i,i) -> a -> P a

instance GetPixel (Image GrayScale Double) where
    type P (Image GrayScale Double) = Double 
    getPixel (fromIntegral -> x, fromIntegral -> y) image = realToFrac $ unsafePerformIO $
             withGenImage image $ \img -> {#call wrapGet32F2D#} img y x

instance  GetPixel (Image RGB Double) where
    type P (Image RGB Double) = (Double,Double,Double) 
    getPixel (fromIntegral -> x, fromIntegral -> y) image 
        = unsafePerformIO $ do 
                     withGenImage image $ \img -> do
                              r <- {#call wrapGet32F2DC#} img y x 0
                              g <- {#call wrapGet32F2DC#} img y x 1
                              b <- {#call wrapGet32F2DC#} img y x 2
                              return (realToFrac r,realToFrac g, realToFrac b)


convertTo :: CInt -> CInt -> BareImage -> BareImage
convertTo code channels img = unsafePerformIO $ creatingBareImage $ do
    res <- {#call wrapCreateImage32F#} w h channels
    withBareImage img $ \cimg -> 
        {#call cvCvtColor#} (castPtr cimg) (castPtr res) code
    return res
 where    
    (fromIntegral -> w,fromIntegral -> h) = getSize img

class CreateImage a where
    create :: (Int,Int) -> IO a


instance CreateImage (Image GrayScale D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 1
instance CreateImage (Image LAB D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGB D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGBA D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 4

instance CreateImage (Image GrayScale D64) where
    create (w,h) = creatingImage $ {#call wrapCreateImage64F#} (fromIntegral w) (fromIntegral h) 1
instance CreateImage (Image LAB D64) where
    create (w,h) = creatingImage $ {#call wrapCreateImage64F#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGB D64) where
    create (w,h) = creatingImage $ {#call wrapCreateImage64F#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGBA D64) where
    create (w,h) = creatingImage $ {#call wrapCreateImage64F#} (fromIntegral w) (fromIntegral h) 4

instance CreateImage (Image GrayScale D8) where
    create (w,h) = creatingImage $ {#call wrapCreateImage8U#} (fromIntegral w) (fromIntegral h) 1
instance CreateImage (Image LAB D8) where
    create (w,h) = creatingImage $ {#call wrapCreateImage8U#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGB D8) where
    create (w,h) = creatingImage $ {#call wrapCreateImage8U#} (fromIntegral w) (fromIntegral h) 3
instance CreateImage (Image RGBA D8) where
    create (w,h) = creatingImage $ {#call wrapCreateImage8U#} (fromIntegral w) (fromIntegral h) 4




emptyCopy img = create (getSize img) 

-- | Save image. This will convert the image to 8 bit one before saving
saveImage :: FilePath -> Image c d -> IO ()
saveImage filename image = do
                           fpi <- imageTo8Bit $ unS image
                           withCString  filename $ \name  -> 
                            withGenBareImage fpi    $ \cvArr ->
							 alloca (\defs -> poke defs 0 >> {#call cvSaveImage #} name cvArr defs >> return ())


getArea :: (IntSized a) => a -> Int
getArea = uncurry (*).getSize

getRegion :: (Int,Int) -> (Int,Int) -> Image c d -> Image c d
getRegion (fromIntegral -> x,fromIntegral -> y) (fromIntegral -> w,fromIntegral -> h) image 
    | x+w <= width && y+h <= height = S . getRegion' (x,y) (w,h) $ unS image
    | otherwise                   = error $ "Region outside image:"
                                            ++ show (getSize image) ++
                                            "/"++show (x+w,y+h)
 where
  (fromIntegral -> width,fromIntegral -> height) = getSize image
    
getRegion' (x,y) (w,h) image = unsafePerformIO $
                               withBareImage image $ \i ->
                                 creatingBareImage ({#call getSubImage#} 
                                                i x y w h)


-- | Tile images by overlapping them on a black canvas.
tileImages image1 image2 (x,y) = unsafePerformIO $
                               withImage image1 $ \i1 ->
                                withImage image2 $ \i2 ->
                                 creatingImage ({#call simpleMergeImages#} 
                                                i1 i2 x y)
-- | Blit image2 onto image1. 
blitFix = blit
blit image1 image2 (x,y) 
    | badSizes  = error $ "Bad blit sizes: " ++ show [(w1,h1),(w2,h2)]++"<-"++show (x,y) 
    | otherwise = withImage image1 $ \i1 ->
                   withImage image2 $ \i2 ->
                    ({#call plainBlit#} i1 i2 (fromIntegral y) (fromIntegral x))
    where 
     ((w1,h1),(w2,h2)) = (getSize image1,getSize image2)
     badSizes = x+w2>w1 || y+h2>h1 || x<0 || y<0

blitM :: (CreateImage (Image c d)) => (Int,Int) -> [((Int,Int),Image c d)] -> Image c d
blitM (rw,rh) imgs = resultPic
    where
     resultPic = unsafePerformIO $ do
                    r <- create (fromIntegral rw,fromIntegral rh) 
                    sequence_ [blit r i (fromIntegral x, fromIntegral y) 
                              | ((x,y),i) <- imgs ]
                    return r


subPixelBlit
  :: Image c d -> Image c d -> (CDouble, CDouble) -> IO ()

subPixelBlit (image1) (image2) (x,y) 
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

unsafeImageTo32F img = unsafePerformIO $ withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure32F #} image)

unsafeImageTo8Bit img = unsafePerformIO $ withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure8U #} image)

imageTo32F img = withGenBareImage img $ \image -> 
                creatingBareImage 
                 ({#call ensure32F #} image)

imageTo8Bit img = withGenBareImage img $ \image -> 
                creatingBareImage 
                 ({#call ensure8U #} image)

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
    cres <- {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 1
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
--setPixel :: (CInt,CInt) -> CDouble -> Image c d -> IO ()
--setPixel (x,y) v image = withGenImage image $ \img ->
--                          {#call wrapSet32F2D#} img y x v


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
montage :: (CreateImage (Image c d)) => (Int,Int) -> Int -> [Image c d] -> Image c d
montage (u',v') space' imgs = resultPic
    where
     space = fromIntegral space'
     (u,v) = (fromIntegral u', fromIntegral v')
     (rw,rh) = (u*xstep,v*ystep) 
     (w,h) = getSize (head imgs)
     (xstep,ystep) = (fromIntegral space + w,fromIntegral space + h)
     edge = space`div`2
     resultPic = unsafePerformIO $ do
                    r <- create (rw,rh)
                    sequence_ [blit r i (edge +  x*xstep, edge + y*ystep) 
                               | x <- [0..u-1] , y <- [0..v-1] 
                               | i <- imgs ]
                    return r

