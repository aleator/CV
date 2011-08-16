{-#LANGUAGE ForeignFunctionInterface#-}
-- |This  module provides slow but functional means for exporting images from and to 
--  CArrays, which can easily be passed into foreign functions.
module CV.Conversions (
     -- Arrays of Double
     copyCArrayToImage
    ,copy8UCArrayToImage
    ,copyImageToCArray
     -- Arrays of Float
    ,copyFCArrayToImage
    ,copyImageToFCArray
    -- * Complex arrays
    ,copyComplexCArrayToImage
    ,copyImageToComplexCArray
    -- * Copying
    ,copyImageToExistingCArray
    -- * Acquiring images from pointers
    ,unsafe8UC_RGBFromPtr
    ,unsafe8UC_BGRFromPtr
    ,acquireImageSlowF'
    ,acquireImageSlow'
    ,acquireImageSlow8URGB'
    ) where

import Complex

import CV.Image
import Data.Word

import Data.Array.CArray
import Data.Array.IArray

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Complex
import System.IO.Unsafe

unsafe8UC_RGBFromPtr :: (Int,Int) -> Ptr Word8 -> IO (Image RGB D8)
unsafe8UC_RGBFromPtr (w,h) ptr = S `fmap`  creatingBareImage (acquireImageSlow8URGB' w h ptr)

unsafe8UC_BGRFromPtr :: (Int,Int) -> Ptr Word8 -> IO (Image RGB D8)
unsafe8UC_BGRFromPtr (w,h) ptr = S `fmap`  creatingBareImage (acquireImageSlow8UBGR' w h ptr)

-- |Copy the contents of a CArray into CV.Image type.
copy8UCArrayToImage :: CArray (Int,Int) Word8 -> Image GrayScale D8
copy8UCArrayToImage carr = S $ unsafePerformIO $
                          creatingBareImage (withCArray carr (acquireImageSlow8U' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)
-- |Copy the contents of a CArray into CV.Image type.
copyCArrayToImage :: CArray (Int,Int) Double -> Image GrayScale D32
copyCArrayToImage carr = S $ unsafePerformIO $
                          creatingBareImage (withCArray carr (acquireImageSlow' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)

-- |Copy CArray of floats to image
copyFCArrayToImage :: CArray (Int,Int) Float -> Image GrayScale D32
copyFCArrayToImage carr = S $ unsafePerformIO $
                          creatingBareImage (withCArray carr (acquireImageSlowF' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)

-- |Copy D32 grayscale image to CArray
copyImageToFCArray :: Image GrayScale D32 -> CArray (Int,Int) Float
copyImageToFCArray (S img) = unsafePerformIO $
         withBareImage img $ \cimg -> 
          createCArray ((0,0),(w-1,h-1)) (exportImageSlowF' cimg) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img




-- |Copy the real part of an array to image
copyComplexCArrayToImage :: CArray (Int,Int) (Complex Double) -> Image GrayScale D32
copyComplexCArrayToImage carr = S $ unsafePerformIO $
                          creatingBareImage (withCArray carr (acquireImageSlowComplex' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)

-- |Copy the contents of a CV.Image into a CArray.
copyImageToCArray :: Image GrayScale D32 -> CArray (Int,Int) Double
copyImageToCArray (S img) = unsafePerformIO $
         withBareImage img $ \cimg -> 
          createCArray ((0,0),(w-1,h-1)) (exportImageSlow' cimg) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img
-- |Copy the contents of CV.Image into a pre-existing CArray.
--
copyImageToExistingCArray (S img) arr = 
         withBareImage img $ \cimg -> 
          withCArray arr $ \carr -> (exportImageSlow' cimg carr) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img

-- |Copy image as a real part of a complex CArray
copyImageToComplexCArray :: Image GrayScale D32 -> CArray (Int,Int) (Complex Double)
copyImageToComplexCArray (S img) = unsafePerformIO $
         withBareImage img $ \cimg -> 
          createCArray ((0,0),(w-1,h-1)) (exportImageSlowComplex' cimg) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img

foreign import ccall safe "CV/cvWrapLeo.h exportImageSlow"
  exportImageSlow' :: ((Ptr (BareImage)) -> ((Ptr Double) -> (IO ())))

foreign import ccall safe "CV/cvWrapLeo.h exportImageSlowF"
  exportImageSlowF' :: ((Ptr (BareImage)) -> ((Ptr Float) -> (IO ())))

foreign import ccall safe "CV/cvWrapLeo.h exportImageSlowComplex"
  exportImageSlowComplex' :: ((Ptr (BareImage)) -> ((Ptr (Complex Double)) -> (IO ())))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlow"
  acquireImageSlow' :: (Int -> (Int -> ((Ptr Double) -> (IO (Ptr (BareImage))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlowF"
  acquireImageSlowF' :: (Int -> (Int -> ((Ptr Float) -> (IO (Ptr (BareImage))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlow8URGB"
  acquireImageSlow8URGB' :: (Int -> (Int -> ((Ptr Word8) -> (IO (Ptr (BareImage))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlow8UBGR"
  acquireImageSlow8UBGR' :: (Int -> (Int -> ((Ptr Word8) -> (IO (Ptr (BareImage))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlow8U"
  acquireImageSlow8U' :: (Int -> (Int -> ((Ptr Word8) -> (IO (Ptr (BareImage))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlowComplex"
  acquireImageSlowComplex' :: (Int -> (Int -> ((Ptr (Complex Double)) -> (IO (Ptr (BareImage))))))

