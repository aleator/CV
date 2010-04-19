{-#LANGUAGE ForeignFunctionInterface#-}

module CV.Conversions where

import Complex

import CV.Image

import Data.Array.CArray
import Data.Array.IArray

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Complex
import System.IO.Unsafe

-- | Copy the contents of a CArray into CV.Image type.
copyCArrayToImage :: CArray (Int,Int) Double -> Image
copyCArrayToImage carr = unsafePerformIO $
                          creatingImage (withCArray carr (acquireImageSlow' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)

copyComplexCArrayToImage :: CArray (Int,Int) (Complex Double) -> Image
copyComplexCArrayToImage carr = unsafePerformIO $
                          creatingImage (withCArray carr (acquireImageSlowComplex' w h))
    where
     ((sx,sy),(ex,ey)) = bounds carr
     (w,h) = (fromIntegral $ ex-sx+1, fromIntegral $ ey-sy+1)

-- | Copy the contents of a CV.Image into a CArray.
copyImageToCArray :: Image -> CArray (Int,Int) Double
copyImageToCArray img = unsafePerformIO $
         withImage img $ \cimg -> 
          createCArray ((0,0),(w-1,h-1)) (exportImageSlow' cimg) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img

copyImageToComplexCArray :: Image -> CArray (Int,Int) (Complex Double)
copyImageToComplexCArray img = unsafePerformIO $
         withImage img $ \cimg -> 
          createCArray ((0,0),(w-1,h-1)) (exportImageSlowComplex' cimg) --({#call exportImageSlow#} cimg)
    where
     (w,h) = getSize img

foreign import ccall safe "CV/cvWrapLeo.h exportImageSlow"
  exportImageSlow' :: ((Ptr (Image)) -> ((Ptr Double) -> (IO ())))

foreign import ccall safe "CV/cvWrapLeo.h exportImageSlowComplex"
  exportImageSlowComplex' :: ((Ptr (Image)) -> ((Ptr (Complex Double)) -> (IO ())))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlow"
  acquireImageSlow' :: (Int -> (Int -> ((Ptr Double) -> (IO (Ptr (Image))))))

foreign import ccall safe "CV/cvWrapLeo.h acquireImageSlowComplex"
  acquireImageSlowComplex' :: (Int -> (Int -> ((Ptr (Complex Double)) -> (IO (Ptr (Image))))))

