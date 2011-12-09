{-#LANGUAGE ForeignFunctionInterface, ViewPatterns,ParallelListComp, FlexibleInstances, FlexibleContexts, TypeFamilies, EmptyDataDecls, ScopedTypeVariables, StandaloneDeriving #-}
#include "cvWrapLEO.h"
module CV.Image (
-- * Basic types 
 Image(..) 
, create
, empty 
, emptyCopy 
, emptyCopy' 
, cloneImage
, withClone 
, withCloneValue 
, CreateImage 

-- * Colour spaces
, ChannelOf 
, GrayScale
, RGB
, RGBA
, RGB_Channel 
, LAB
, LAB_Channel 
, D32 
, D64 
, D8 
, Tag 
, lab 
, rgba 
, rgb 
, composeMultichannelImage 

-- * IO operations
, Loadable 
, saveImage 
, loadColorImage 
, loadImage 

-- * Pixel level access 
, GetPixel(..)
, getAllPixels 
, getAllPixelsRowMajor 
, setPixel 
, setPixel8U 
, mapImageInplace 

-- * Image information
, ImageDepth
, Sized(..)
, getArea 
, getChannel
, getImageChannels 
, getImageDepth 
, getImageInfo 

-- * ROI's, COI's and subregions
, setCOI 
, setROI 
, resetROI 
, getRegion 
, withIOROI 
, withROI 

-- * Blitting
, blendBlit 
, blit 
, blitM 
, subPixelBlit 
, safeBlit 
, montage 
, tileImages 

-- * Conversions
, rgbToGray 
, rgbToLab 
, unsafeImageTo32F 
, unsafeImageTo8Bit 

-- * Low level access operations
, BareImage(..)
, creatingImage 
, unImage 
, unS 
, withGenBareImage 
, withBareImage 
, creatingBareImage
, withGenImage 
, withImage 
, ensure32F

) where

import System.Posix.Files
import System.Mem

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent 
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
import Control.Monad



-- Colorspaces
data GrayScale
data RGB
data RGB_Channel = Red | Green | Blue deriving (Eq,Ord,Enum)
data RGBA
data LAB
data LAB_Channel = LAB_L | LAB_A | LAB_B deriving (Eq,Ord,Enum)
type family ChannelOf a :: *
type instance ChannelOf RGB_Channel = RGB
type instance ChannelOf LAB_Channel = LAB

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

freeBareImage ptr = with ptr {#call cvReleaseImage#}

--foreign import ccall "& wrapReleaseImage" releaseImage :: FinalizerPtr BareImage

instance NFData (Image a b) where
    rnf a@(S (BareImage fptr)) = (unsafeForeignPtrToPtr) fptr `seq` a `seq` ()-- This might also need peek?


creatingImage fun = do
              iptr <- fun
--              {#call incrImageC#} -- Uncomment this line to get statistics of number of images allocated by ghc
              fptr <- newForeignPtr iptr (freeBareImage iptr) 
              return . S . BareImage $ fptr

creatingBareImage fun = do
              iptr <- fun
--              {#call incrImageC#} -- Uncomment this line to get statistics of number of images allocated by ghc
              fptr <- newForeignPtr iptr (freeBareImage iptr)
              return . BareImage $ fptr

unImage (S (BareImage fptr)) = fptr

data Tag tp;
rgb = undefined :: Tag RGB
rgba = undefined :: Tag RGBA
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

class Loadable a where
    readFromFile :: FilePath -> IO a


instance Loadable ((Image GrayScale D32)) where
    readFromFile fp = do
        e <- loadImage fp
        case e of
         Just i -> return i
         Nothing -> fail $ "Could not load "++fp

instance Loadable ((Image RGB D32)) where
    readFromFile fp = do
        e <- loadColorImage fp
        case e of
         Just i -> return i
         Nothing -> fail $ "Could not load "++fp

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

class Sized a where
    type Size a :: *
    getSize :: a -> Size a

instance Sized BareImage where
    type Size BareImage = (Int,Int)
   -- getSize :: (Integral a, Integral b) => Image c d -> (a,b)
    getSize image = unsafePerformIO $ withBareImage image $ \i -> do
                 w <- {#call getImageWidth#} i
                 h <- {#call getImageHeight#} i
                 return (fromIntegral w,fromIntegral h)

instance Sized (Image c d) where
    type Size (Image c d) = (Int,Int)
    getSize = getSize . unS


cvRGBtoGRAY = 7 :: CInt-- NOTE: This will break.
cvRGBtoLAB = 45 :: CInt-- NOTE: This will break.


rgbToLab :: Image RGB D32 -> Image LAB D32
rgbToLab = S . convertTo cvRGBtoLAB 3 . unS

rgbToGray :: Image RGB D32 -> Image GrayScale D32
rgbToGray = S . convertTo cvRGBtoGRAY 1 . unS


class GetPixel a where
    type P a :: *
    getPixel   :: (Int,Int) -> a -> P a

-- #define FGET(img,x,y) (((float *)((img)->imageData + (y)*(img)->widthStep))[(x)])
instance GetPixel (Image GrayScale D32) where
    type P (Image GrayScale D32) = D32 
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         peek (castPtr (d`plusPtr` (y*(fromIntegral s) +x*sizeOf (0::Float))):: Ptr Float)

{-#INLINE getPixelOld#-}
getPixelOld (fromIntegral -> x, fromIntegral -> y) image = realToFrac $ unsafePerformIO $
         withGenImage image $ \img -> {#call wrapGet32F2D#} img y x

-- #define UGETC(img,color,x,y) (((uint8_t *)((img)->imageData + (y)*(img)->widthStep))[(x)*3+(color)])
instance GetPixel (Image RGB D32) where
    type P (Image RGB D32) = (D32,D32,D32) 
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: Float)
                                         r <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         b <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)

getPixelOldRGB (fromIntegral -> x, fromIntegral -> y) image 
        = unsafePerformIO $ do 
                     withGenImage image $ \img -> do
                              r <- {#call wrapGet32F2DC#} img y x 0
                              g <- {#call wrapGet32F2DC#} img y x 1
                              b <- {#call wrapGet32F2DC#} img y x 2
                              return (realToFrac r,realToFrac g, realToFrac b)

-- | Perform (a destructive) inplace map of the image. This should be wrapped inside 
-- withClone or an image operation 
mapImageInplace :: (P (Image GrayScale D32) -> P (Image GrayScale D32)) 
            -> Image GrayScale D32 
            -> IO ()
mapImageInplace f image = withGenImage image $ \c_i -> do
             d <- {#get IplImage->imageData#} c_i
             s <- {#get IplImage->widthStep#} c_i
             let (w,h) = getSize image
                 cs = fromIntegral s
                 fs = sizeOf (undefined :: Float)
             forM_ [(x,y) | x<-[0..w-1], y <- [0..h-1]] $ \(x,y) -> do
                   v <- peek (castPtr (d `plusPtr` (y*cs+x*fs))) 
                   poke (castPtr (d `plusPtr` (y*cs+x*fs))) (f v)
             

instance  GetPixel (Image RGB D8) where
    type P (Image RGB D8) = (D8,D8,D8) 
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: D8)
                                         r <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         b <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)


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



empty :: (CreateImage (Image a b)) => (Int,Int) -> (Image a b)
empty size = unsafePerformIO $ create size 

emptyCopy :: (CreateImage (Image a b)) => Image a b -> IO (Image a b)
emptyCopy img = create (getSize img) 

emptyCopy' :: (CreateImage (Image a b)) => Image a b -> (Image a b)
emptyCopy' img = unsafePerformIO $ create (getSize img) 

-- | Save image. This will convert the image to 8 bit one before saving
saveImage :: FilePath -> Image c d -> IO ()
saveImage filename image = do
                           fpi <- imageTo8Bit $ unS image
                           withCString  filename $ \name  -> 
                            withGenBareImage fpi    $ \cvArr ->
							 alloca (\defs -> poke defs 0 >> {#call cvSaveImage #} name cvArr defs >> return ())


getArea :: (Sized a,Num b, Size a ~ (b,b)) => a -> b
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

withClone
  :: Image channels depth
     -> (Image channels depth -> IO ())
     -> IO (Image channels depth)
withClone img fun = do 
                result <- cloneImage img
                fun result
                return result

withCloneValue
  :: Image channels depth
     -> (Image channels depth -> IO a)
     -> IO a
withCloneValue img fun = do 
                result <- cloneImage img
                r <- fun result
                return r

unsafeImageTo32F :: Image c d -> Image c D32
unsafeImageTo32F img = unsafePerformIO $ withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure32F #} image)

unsafeImageTo8Bit :: Image cspace a -> Image cspace D8
unsafeImageTo8Bit img = unsafePerformIO $ withGenImage img $ \image -> 
                creatingImage 
                 ({#call ensure8U #} image)

imageTo32F img = withGenBareImage img $ \image -> 
                creatingBareImage 
                 ({#call ensure32F #} image)

imageTo8Bit img = withGenBareImage img $ \image -> 
                creatingBareImage 
                 ({#call ensure8U #} image)
#c
enum ImageDepth {
     Depth32F = IPL_DEPTH_32F,
     Depth64F = IPL_DEPTH_64F,
     Depth8U  = IPL_DEPTH_8U, 
     Depth8S  = IPL_DEPTH_8S, 
     Depth16U  = IPL_DEPTH_16U, 
     Depth16S  = IPL_DEPTH_16S,
     Depth32S  = IPL_DEPTH_32S
     };
#endc
 
{#enum ImageDepth {}#}

deriving instance Show ImageDepth

getImageDepth :: Image c d -> IO ImageDepth
getImageDepth i = withImage i $ \c_img -> {#get IplImage->depth #} c_img >>= return.toEnum.fromIntegral
getImageChannels i = withImage i $ \c_img -> {#get IplImage->nChannels #} c_img

getImageInfo x = do
    let s = getSize x
    d <- getImageDepth x
    c <- getImageChannels x
    return (s,d,c)


-- Manipulating regions of interest:
setROI (fromIntegral -> x,fromIntegral -> y) 
       (fromIntegral -> w,fromIntegral -> h) 
       image = withImage image $ \i -> 
                            {#call wrapSetImageROI#} i x y w h

resetROI image = withImage image $ \i ->
                  {#call cvResetImageROI#} i

setCOI chnl image = withImage image $ \i -> 
                            {#call cvSetImageCOI#} i (fromIntegral chnl)
resetCOI image = withImage image $ \i ->
                  {#call cvSetImageCOI#} i 0


-- #TODO: Replace the Int below with proper channel identifier
getChannel :: (Enum a) => a -> Image (ChannelOf a) d -> Image GrayScale d
getChannel no image = unsafePerformIO $ creatingImage $ do
    let (w,h) = getSize image
    setCOI (1+fromEnum no) image
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

withROI :: (Int, Int) -> (Int, Int) -> Image c d -> (Image c d -> a) -> a
withROI pos size image op = unsafePerformIO $ do
                        setROI pos size image
                        let x = op image -- BUG
                        resetROI image
                        return x


-- | Manipulate image pixels. This is slow, ugly and should be avoided
--setPixel :: (CInt,CInt) -> CDouble -> Image c d -> IO ()
{-#INLINE setPixelOld#-}
setPixelOld :: (Int,Int) -> D32 -> Image GrayScale D32 -> IO ()
setPixelOld (x,y) v image = withGenImage image $ \img ->
                          {#call wrapSet32F2D#} img (fromIntegral y) (fromIntegral x) (realToFrac v)

{-#INLINE setPixel#-}
setPixel :: (Int,Int) -> D32 -> Image GrayScale D32 -> IO ()
setPixel (x,y) v image = withGenImage image $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         poke (castPtr (d`plusPtr` (y*(fromIntegral s) 
                                              + x*sizeOf (0::Float))):: Ptr Float)
                                              v

{-#INLINE setPixel8U#-}
setPixel8U :: (Int,Int) -> Word8 -> Image GrayScale D8 -> IO ()
setPixel8U (x,y) v image = withGenImage image $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         poke (castPtr (d`plusPtr` (y*(fromIntegral s) 
                                              + x*sizeOf (0::Word8))):: Ptr Word8)
                                              v


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
montage (u',v') space' imgs 
    | u'*v' /= (length imgs) = error ("Montage mismatch: "++show (u,v, length imgs))
    | otherwise              = resultPic
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
                               | y <- [0..v-1] , x <- [0..u-1] 
                               | i <- imgs ]
                    return r

