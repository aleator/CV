{-#LANGUAGE ForeignFunctionInterface, ViewPatterns,ParallelListComp, FlexibleInstances, FlexibleContexts, TypeFamilies, EmptyDataDecls, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable, UndecidableInstances #-}
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
, Complex
, RGB
, RGBA
, RGB_Channel(..)
, LAB
, LAB_Channel(..)
, D32
, D64
, D8
, Tag
, lab
, rgba
, rgb
, compose
, composeMultichannelImage

-- * IO operations
, Loadable(..)
, saveImage
, loadColorImage
, loadImage

-- * Pixel level access
, GetPixel(..)
, SetPixel(..)
, getAllPixels
, getAllPixelsRowMajor
, mapImageInplace

-- * Image information
, ImageDepth
, Sized(..)
, biggerThan
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
, grayToRGB
, rgbToLab
, bgrToRgb
, rgbToBgr
, cloneTo64F
, unsafeImageTo32F 
, unsafeImageTo64F 
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
, imageFPTR
, ensure32F

-- * Extended error handling
, setCatch
, CvException
, CvSizeError(..)
, CvIOError(..)
) where

import System.Mem
import System.Directory
import System.FilePath

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.ForeignPtr hiding (newForeignPtr,unsafeForeignPtrToPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Control.Parallel.Strategies
import Control.DeepSeq

import CV.Bindings.Error

import Data.Maybe(catMaybes)
import Data.List(genericLength)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import qualified Data.Complex as C
import Control.Monad
import Control.Exception
import Data.Data
import Data.Typeable

import Utils.GeometryClass




-- Colorspaces

-- | Single channel grayscale image
data GrayScale
data Complex
data RGB
data RGB_Channel = Red | Green | Blue deriving (Eq,Ord,Enum)

data BGR

data LAB
data RGBA
data LAB_Channel = LAB_L | LAB_A | LAB_B deriving (Eq,Ord,Enum)

-- | Type family for expressing which channels a colorspace contains. This needs to be fixed wrt. the BGR color space.
type family ChannelOf a :: *
type instance ChannelOf RGB_Channel = RGB
type instance ChannelOf LAB_Channel = LAB

-- Bit Depths
type D8  = Word8
type D32 = Float
type D64 = Double

-- | The type for Images
newtype Image channels depth = S BareImage

-- | Remove typing info from an image
unS (S i) = i -- Unsafe and ugly

imageFPTR :: Image c d -> ForeignPtr BareImage
imageFPTR (S (BareImage fptr)) = fptr

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

-- | Typeclass for elements that are build from component elements. For example,
--   RGB images can be constructed from three grayscale images.
class Composes a where
   type Source a :: *
   compose :: Source a -> a

instance (CreateImage (Image RGBA a)) => Composes (Image RGBA a) where
   type Source (Image RGBA a) = (Image GrayScale a, Image GrayScale a
                               ,Image GrayScale a, Image GrayScale a)
   compose (r,g,b,a) = composeMultichannelImage (Just b) (Just g) (Just r) (Just a) rgba

instance (CreateImage (Image RGB a)) => Composes (Image RGB a) where
   type Source (Image RGB a) = (Image GrayScale a, Image GrayScale a, Image GrayScale a)
   compose (r,g,b) = composeMultichannelImage (Just b) (Just g) (Just r) Nothing rgb

instance (CreateImage (Image LAB a)) => Composes (Image LAB a) where
   type Source (Image LAB a) = (Image GrayScale a, Image GrayScale a, Image GrayScale a)
   compose (l,a,b) = composeMultichannelImage (Just l) (Just a) (Just b) Nothing lab

{-# DEPRECATED composeMultichannelImage "This is unsafe. Use compose instead" #-}
composeMultichannelImage :: (CreateImage (Image tp a)) => Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Tag tp -> Image tp a
composeMultichannelImage = composeMultichannel

composeMultichannel :: (CreateImage (Image tp a)) => Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Maybe (Image GrayScale a) -> Tag tp -> Image tp a
composeMultichannel (c2)
                         (c1)
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


-- | Typeclass for CV items that can be read from file. Mainly images at this point.
class Loadable a where
    readFromFile :: FilePath -> IO a


instance Loadable ((Image GrayScale D32)) where
    readFromFile fp = do
        e <- loadImage fp
        case e of
         Just i -> return i
         Nothing -> throw $ CvIOError $ "Could not load "++fp

instance Loadable ((Image RGB D32)) where
    readFromFile fp = do
        e <- loadColorImage8 fp
        case e of
         Just i -> return $ unsafeImageTo32F $ bgrToRgb i
         Nothing -> throw $ CvIOError $ "Could not load "++fp

instance Loadable ((Image RGB D8)) where
    readFromFile fp = do
        e <- loadColorImage8 fp
        case e of
         Just i -> return $ bgrToRgb i
         Nothing -> throw $ CvIOError $ "Could not load "++fp

instance Loadable ((Image GrayScale D8)) where
    readFromFile fp = do
        e <- loadImage8 fp
        case e of
         Just i -> return i
         Nothing -> throw $ CvIOError $ "Could not load "++fp


-- | This function loads and converts image to an arbitrary format. Notice that it is
--   polymorphic enough to cause run time errors if the declared and actual types of the
--   images do not match. Use with care.
unsafeloadUsing x p n = do
              exists <- doesFileExist n
              if not exists then return Nothing
                            else do
                              i <- withCString n $ \name ->
                                     creatingBareImage ({#call cvLoadImage #} name p)
                              bw <- x i
                              return . Just . S $ bw

loadImage :: FilePath -> IO (Maybe (Image GrayScale D32))
loadImage = unsafeloadUsing imageTo32F 0
loadImage8 :: FilePath -> IO (Maybe (Image GrayScale D8))
loadImage8 = unsafeloadUsing imageTo8Bit 0
loadColorImage :: FilePath -> IO (Maybe (Image BGR D32))
loadColorImage = unsafeloadUsing imageTo32F 1
loadColorImage8 :: FilePath -> IO (Maybe (Image BGR D8))
loadColorImage8 = unsafeloadUsing imageTo8Bit 1



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


#c
enum CvtFlags {
    CvtFlip   = CV_CVTIMG_FLIP,
    CvtSwapRB = CV_CVTIMG_SWAP_RB
     };
#endc

#c
enum CvtCodes {
    CV_BGR2BGRA    =0,
    CV_RGB2RGBA    =CV_BGR2BGRA,

    CV_BGRA2BGR    =1,
    CV_RGBA2RGB    =CV_BGRA2BGR,

    CV_BGR2RGBA    =2,
    CV_RGB2BGRA    =CV_BGR2RGBA,

    CV_RGBA2BGR    =3,
    CV_BGRA2RGB    =CV_RGBA2BGR,

    CV_BGR2RGB     =4,
    CV_RGB2BGR     =CV_BGR2RGB,

    CV_BGRA2RGBA   =5,
    CV_RGBA2BGRA   =CV_BGRA2RGBA,

    CV_BGR2GRAY    =6,
    CV_RGB2GRAY    =7,
    CV_GRAY2BGR    =8,
    CV_GRAY2RGB    =CV_GRAY2BGR,
    CV_GRAY2BGRA   =9,
    CV_GRAY2RGBA   =CV_GRAY2BGRA,
    CV_BGRA2GRAY   =10,
    CV_RGBA2GRAY   =11,

    CV_BGR2BGR565  =12,
    CV_RGB2BGR565  =13,
    CV_BGR5652BGR  =14,
    CV_BGR5652RGB  =15,
    CV_BGRA2BGR565 =16,
    CV_RGBA2BGR565 =17,
    CV_BGR5652BGRA =18,
    CV_BGR5652RGBA =19,

    CV_GRAY2BGR565 =20,
    CV_BGR5652GRAY =21,

    CV_BGR2BGR555  =22,
    CV_RGB2BGR555  =23,
    CV_BGR5552BGR  =24,
    CV_BGR5552RGB  =25,
    CV_BGRA2BGR555 =26,
    CV_RGBA2BGR555 =27,
    CV_BGR5552BGRA =28,
    CV_BGR5552RGBA =29,

    CV_GRAY2BGR555 =30,
    CV_BGR5552GRAY =31,

    CV_BGR2XYZ     =32,
    CV_RGB2XYZ     =33,
    CV_XYZ2BGR     =34,
    CV_XYZ2RGB     =35,

    CV_BGR2YCrCb   =36,
    CV_RGB2YCrCb   =37,
    CV_YCrCb2BGR   =38,
    CV_YCrCb2RGB   =39,

    CV_BGR2HSV     =40,
    CV_RGB2HSV     =41,

    CV_BGR2Lab     =44,
    CV_RGB2Lab     =45,

    CV_BayerBG2BGR =46,
    CV_BayerGB2BGR =47,
    CV_BayerRG2BGR =48,
    CV_BayerGR2BGR =49,

    CV_BayerBG2RGB =CV_BayerRG2BGR,
    CV_BayerGB2RGB =CV_BayerGR2BGR,
    CV_BayerRG2RGB =CV_BayerBG2BGR,
    CV_BayerGR2RGB =CV_BayerGB2BGR,

    CV_BGR2Luv     =50,
    CV_RGB2Luv     =51,
    CV_BGR2HLS     =52,
    CV_RGB2HLS     =53,

    CV_HSV2BGR     =54,
    CV_HSV2RGB     =55,

    CV_Lab2BGR     =56,
    CV_Lab2RGB     =57,
    CV_Luv2BGR     =58,
    CV_Luv2RGB     =59,
    CV_HLS2BGR     =60,
    CV_HLS2RGB     =61,

    CV_BayerBG2BGR_VNG =62,
    CV_BayerGB2BGR_VNG =63,
    CV_BayerRG2BGR_VNG =64,
    CV_BayerGR2BGR_VNG =65,

    CV_BayerBG2RGB_VNG =CV_BayerRG2BGR_VNG,
    CV_BayerGB2RGB_VNG =CV_BayerGR2BGR_VNG,
    CV_BayerRG2RGB_VNG =CV_BayerBG2BGR_VNG,
    CV_BayerGR2RGB_VNG =CV_BayerGB2BGR_VNG,

    CV_BGR2HSV_FULL = 66,
    CV_RGB2HSV_FULL = 67,
    CV_BGR2HLS_FULL = 68,
    CV_RGB2HLS_FULL = 69,

    CV_HSV2BGR_FULL = 70,
    CV_HSV2RGB_FULL = 71,
    CV_HLS2BGR_FULL = 72,
    CV_HLS2RGB_FULL = 73,

    CV_LBGR2Lab     = 74,
    CV_LRGB2Lab     = 75,
    CV_LBGR2Luv     = 76,
    CV_LRGB2Luv     = 77,

    CV_Lab2LBGR     = 78,
    CV_Lab2LRGB     = 79,
    CV_Luv2LBGR     = 80,
    CV_Luv2LRGB     = 81,

    CV_BGR2YUV      = 82,
    CV_RGB2YUV      = 83,
    CV_YUV2BGR      = 84,
    CV_YUV2RGB      = 85,

    CV_COLORCVT_MAX  =100
};
#endc

{#enum CvtCodes {}#}

{#enum CvtFlags {}#}


rgbToLab :: Image RGB D32 -> Image LAB D32
rgbToLab = S . convertTo cvRGBtoLAB 3 . unS

rgbToGray :: Image RGB D32 -> Image GrayScale D32
rgbToGray = S . convertTo cvRGBtoGRAY 1 . unS

grayToRGB :: Image GrayScale D32 -> Image RGB D32
grayToRGB = S . convertTo (fromIntegral . fromEnum $ CV_GRAY2BGR) 3 . unS


bgrToRgb :: Image BGR D8 -> Image RGB D8
bgrToRgb = S . swapRB . unS

rgbToBgr :: Image RGB D8 -> Image BGR D8
rgbToBgr = S . swapRB . unS

swapRB :: BareImage -> BareImage
swapRB img = unsafePerformIO $ do
    res <- cloneBareImage img
    withBareImage img $ \cimg ->
     withBareImage res $ \cres ->
        {#call cvConvertImage#} (castPtr cimg) (castPtr cres) (fromIntegral . fromEnum $ CvtSwapRB)
    return res


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

instance GetPixel (Image GrayScale D8) where
    type P (Image GrayScale D8) = D8
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         peek (castPtr (d`plusPtr` (y*(fromIntegral s) +x*sizeOf (0::Word8))):: Ptr Word8)

instance GetPixel (Image Complex D32) where
    type P (Image Complex D32) = C.Complex D32
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: Float)
                                         re <- peek (castPtr (d`plusPtr` (y*cs + x*2*fs)))
                                         im <- peek (castPtr (d`plusPtr` (y*cs +(x*2+1)*fs)))
                                         return (re C.:+ im)

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
                                         b <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         r <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)
instance GetPixel (Image BGR D32) where
    type P (Image BGR D32) = (D32,D32,D32)
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: Float)
                                         b <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         r <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)
instance  GetPixel (Image BGR D8) where
    type P (Image BGR D8) = (D8,D8,D8)
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: D8)
                                         b <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         r <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)

instance  GetPixel (Image RGB D8) where
    type P (Image RGB D8) = (D8,D8,D8)
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: D8)
                                         b <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         g <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         r <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (r,g,b)

instance GetPixel (Image LAB D32) where
    type P (Image LAB D32) = (D32,D32,D32)
    {-#INLINE getPixel#-}
    getPixel (x,y) i = unsafePerformIO $
                        withGenImage i $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: Float)
                                         l <- peek (castPtr (d`plusPtr` (y*cs +x*3*fs)))
                                         a <- peek (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs)))
                                         b <- peek (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs)))
                                         return (l,a,b)

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




convertTo :: CInt -> CInt -> BareImage -> BareImage
convertTo code channels img = unsafePerformIO $ creatingBareImage $ do
    res <- {#call wrapCreateImage32F#} w h channels
    withBareImage img $ \cimg ->
        {#call cvCvtColor#} (castPtr cimg) (castPtr res) code
    return res
 where
    (fromIntegral -> w,fromIntegral -> h) = getSize img

-- | Class for images that exist.
class CreateImage a where
    -- | Create an image from size
    create :: (Int,Int) -> IO a


instance CreateImage (Image GrayScale D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 1
instance CreateImage (Image Complex D32) where
    create (w,h) = creatingImage $ {#call wrapCreateImage32F#} (fromIntegral w) (fromIntegral h) 2
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



-- | Allocate a new empty image
empty :: (CreateImage (Image a b)) => (Int,Int) -> (Image a b)
empty size = unsafePerformIO $ create size

-- | Allocate a new image that of the same size and type as the exemplar image given.
emptyCopy :: (CreateImage (Image a b)) => Image a b -> (Image a b)
emptyCopy img = unsafePerformIO $ create (getSize img)

emptyCopy' :: (CreateImage (Image a b)) => Image a b -> IO (Image a b)
emptyCopy' img = create (getSize img)

-- | Save image. This will convert the image to 8 bit one before saving
class Save a where
    save :: FilePath -> a -> IO ()

instance Save (Image BGR D32) where
    save filename image = primitiveSave filename (unS . unsafeImageTo8Bit $ image)

instance Save (Image RGB D32) where
    save filename image = primitiveSave filename (swapRB . unS . unsafeImageTo8Bit $ image)

instance Save (Image RGB D8) where
    save filename image = primitiveSave filename  (swapRB . unS $ image)

instance Save (Image GrayScale D8) where
    save filename image = primitiveSave filename (unS $ image)

instance Save (Image GrayScale D32) where
    save filename image = primitiveSave filename (unS . unsafeImageTo8Bit $ image)

primitiveSave :: FilePath -> BareImage -> IO ()
primitiveSave filename fpi = do
       exists <- doesDirectoryExist (takeDirectory filename)
       when (not exists) $ throw (CvIOError $ "Directory does not exist: " ++ (takeDirectory filename))
       withCString  filename $ \name  ->
        withGenBareImage fpi    $ \cvArr ->
         alloca (\defs -> poke defs 0 >> {#call cvSaveImage #} name cvArr defs >> return ())

saveImage :: (Save (Image c d)) => FilePath -> Image c d ->  IO ()
saveImage = save

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
blit :: Image GrayScale D32 -> Image GrayScale D32 -> (Int,Int) -> IO ()
blit image1 image2 (x,y)
    | badSizes  = error $ "Bad blit sizes: " ++ show [(w1,h1),(w2,h2)]++"<-"++show (x,y)
    | otherwise = withImage image1 $ \i1 ->
                   withImage image2 $ \i2 ->
                    ({#call plainBlit#} i1 i2 (fromIntegral y) (fromIntegral x))
    where
     ((w1,h1),(w2,h2)) = (getSize image1,getSize image2)
     badSizes = x+w2>w1 || y+h2>h1 || x<0 || y<0

-- blitM :: (CreateImage (Image c d)) => (Int,Int) -> [((Int,Int),Image c d)] -> Image c d
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
                                   ({#call alphaBlit#} i1 i1a i2 i2a y x)


-- | Create a copy of an image
cloneImage :: Image a b -> IO (Image a b)
cloneImage img = withGenImage img $ \image ->
                    creatingImage ({#call cvCloneImage #} image)

-- | Create a copy of a non-types image
cloneBareImage :: BareImage -> IO BareImage
cloneBareImage img = withGenBareImage img $ \image ->
                    creatingBareImage ({#call cvCloneImage #} image)

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

cloneTo64F :: Image c d -> IO (Image c D64)
cloneTo64F img = withGenImage img $ \image ->
                creatingImage
                 ({#call ensure64F #} image)

unsafeImageTo64F :: Image c d -> Image c D64
unsafeImageTo64F img = unsafePerformIO $ withGenImage img $ \image ->
                creatingImage
                 ({#call ensure64F #} image)

unsafeImageTo32F :: Image c d -> Image c D32
unsafeImageTo32F img = unsafePerformIO $ withGenImage img $ \image ->
                creatingImage
                 ({#call ensure32F #} image)

unsafeImageTo8Bit :: Image cspace a -> Image cspace D8
unsafeImageTo8Bit img = unsafePerformIO $ withGenImage img $ \image ->
                creatingImage
                 ({#call ensure8U #} image)

--toD32 :: Image c d -> Image c D32
--toD32 i =
--  unsafePerformIO $
--    withImage i $ \i_ptr ->
--      creatingImage


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

class SetPixel a where
   type SP a :: *
   setPixel :: (Int,Int) -> SP a -> a -> IO ()

instance SetPixel (Image GrayScale D32) where
   type SP (Image GrayScale D32) = D32
   {-#INLINE setPixel#-}
   setPixel (x,y) v image = withGenImage image $ \c_i -> do
                                  d <- {#get IplImage->imageData#} c_i
                                  s <- {#get IplImage->widthStep#} c_i
                                  poke (castPtr (d`plusPtr` (y*(fromIntegral s)
                                       + x*sizeOf (0::Float))):: Ptr Float)
                                       v

instance SetPixel (Image GrayScale D8) where
   type SP (Image GrayScale D8) = D8
   {-#INLINE setPixel#-}
   setPixel (x,y) v image = withGenImage image $ \c_i -> do
                             d <- {#get IplImage->imageData#} c_i
                             s <- {#get IplImage->widthStep#} c_i
                             poke (castPtr (d`plusPtr` (y*(fromIntegral s)
                                  + x*sizeOf (0::Word8))):: Ptr Word8)
                                  v

instance SetPixel (Image RGB D32) where
    type SP (Image RGB D32) = (D32,D32,D32)
    {-#INLINE setPixel#-}
    setPixel (x,y) (r,g,b) image = withGenImage image $ \c_i -> do
                                         d <- {#get IplImage->imageData#} c_i
                                         s <- {#get IplImage->widthStep#} c_i
                                         let cs = fromIntegral s
                                             fs = sizeOf (undefined :: Float)
                                         poke (castPtr (d`plusPtr` (y*cs +x*3*fs)))     b
                                         poke (castPtr (d`plusPtr` (y*cs +(x*3+1)*fs))) g
                                         poke (castPtr (d`plusPtr` (y*cs +(x*3+2)*fs))) r

instance SetPixel (Image Complex D32) where
    type SP (Image Complex D32) = C.Complex D32
    {-#INLINE setPixel#-}
    setPixel (x,y) (re C.:+ im) image = withGenImage image $ \c_i -> do
                             d <- {#get IplImage->imageData#} c_i
                             s <- {#get IplImage->widthStep#} c_i
                             let cs = fromIntegral s
                                 fs = sizeOf (undefined :: Float)
                             poke (castPtr (d`plusPtr` (y*cs + x*2*fs))) re
                             poke (castPtr (d`plusPtr` (y*cs + (x*2+1)*fs))) im



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
montage :: (CreateImage (Image GrayScale D32)) => (Int,Int) -> Int -> [Image GrayScale D32] -> Image GrayScale D32
montage (u',v') space' imgs
    | u'*v' < (length imgs) = error ("Montage mismatch: "++show (u,v, length imgs))
    | otherwise              = resultPic
    where
     space = fromIntegral space'
     (u,v) = (fromIntegral u', fromIntegral v')
     (rw,rh) = (u*xstep,v*ystep)
     (w,h) = foldl (\(mx,my) (x,y) -> (max mx x, max my y)) (0,0) $ map getSize imgs
     (xstep,ystep) = (fromIntegral space + w,fromIntegral space + h)
     edge = space`div`2
     resultPic = unsafePerformIO $ do
                    r <- create (rw,rh)
                    sequence_ [blit r i (edge +  x*xstep, edge + y*ystep)
                               | y <- [0..v-1] , x <- [0..u-1]
                               | i <- imgs ]
                    return r

data CvException = CvException Int String String String Int
     deriving (Show, Typeable)

data CvIOError = CvIOError String deriving (Show,Typeable)
data CvSizeError = CvSizeError String deriving (Show,Typeable)

instance Exception CvException
instance Exception CvIOError
instance Exception CvSizeError

setCatch = do
   let catch i cstr1 cstr2 cstr3 j = do
         func <- peekCString cstr1
         msg  <- peekCString cstr2
         file <- peekCString cstr3
         throw (CvException (fromIntegral i) func msg file (fromIntegral j))
         return 0
   cb <- mk'CvErrorCallback catch
   c'cvRedirectError cb nullPtr nullPtr
   c'cvSetErrMode c'CV_ErrModeSilent

