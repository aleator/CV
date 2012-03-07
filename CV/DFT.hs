{-# LANGUAGE ScopedTypeVariables #-}
module CV.DFT where

import CV.Bindings.Core
import CV.Bindings.ImgProc
import CV.Bindings.Types
import CV.Image
import CV.Operations

import Foreign.Ptr (castPtr,nullPtr)
import System.IO.Unsafe
import Data.Complex

type I32 = Image GrayScale D32
type Idft32 = Image DFT D32
data Ipolar32 = Ipolar32 (Complex D32)

dft :: Image GrayScale d -> Image DFT D32
dft i = unsafePerformIO $ do
  --n::(Image GrayScale D32) <- create (w', h')
  --n <- copyMakeBorder i 0 (h'-h) 0 (w'-w) BorderReplicate 0
  z::(Image GrayScale D32) <- create (w, h)
  d::(Image DFT D32) <- create (w, h)
  withImage i $ \i_ptr ->
    withImage z $ \z_ptr ->
      withImage d $ \d_ptr -> do
        c'cvMerge (castPtr i_ptr) (castPtr z_ptr) nullPtr nullPtr (castPtr d_ptr)
        c'cvDFT (castPtr d_ptr) (castPtr d_ptr) c'CV_DXT_FORWARD (fromIntegral 0)
        c'swapQuadrants (castPtr d_ptr)
        return d
  where
    (w,h) = getSize i
    --w' = fromIntegral $ c'cvGetOptimalDFTSize (fromIntegral w)
    --h' = fromIntegral $ c'cvGetOptimalDFTSize (fromIntegral h)

idft :: Image DFT D32 -> Image GrayScale D32
idft d = unsafePerformIO $ do
  n::(Image GrayScale D32) <- create s
  --z::(Image GrayScale D32) <- create s
  withImage d $ \d_ptr ->
    withImage n $ \n_ptr -> do
      --withImage z $ \z_ptr -> do
        c'swapQuadrants (castPtr d_ptr)
        c'cvDFT (castPtr d_ptr) (castPtr n_ptr) c'CV_DXT_INV_SCALE (fromIntegral 0)
        --c'cvSplit (castPtr d_ptr) (castPtr n_ptr) (castPtr z_ptr) nullPtr nullPtr
        return n
  where
    s = getSize d

dftSplit :: Image DFT D32 -> (Image GrayScale D32, Image GrayScale D32)
dftSplit d = unsafePerformIO $ do
  re::(Image GrayScale D32) <- create (w, h)
  im::(Image GrayScale D32) <- create (w, h)
  withImage d $ \d_ptr ->
    withImage re $ \re_ptr ->
      withImage im $ \im_ptr -> do
        c'cvSplit (castPtr d_ptr) (castPtr re_ptr) (castPtr im_ptr) nullPtr nullPtr
        return (re,im)
  where
    (w,h) = getSize d

dftMerge :: (Image GrayScale D32, Image GrayScale D32) -> Image DFT D32
dftMerge (re,im) = unsafePerformIO $ do
  d::(Image DFT D32) <- create (w, h)
  withImage re $ \re_ptr ->
    withImage im $ \im_ptr ->
      withImage d $ \d_ptr -> do
        c'cvMerge (castPtr re_ptr) (castPtr im_ptr) nullPtr nullPtr (castPtr d_ptr)
  return d
  where
    (w,h) = getSize re

dftToPolar :: Image DFT D32 -> (Image GrayScale D32, Image GrayScale D32)
dftToPolar d = unsafePerformIO $ do
  mag::(Image GrayScale D32) <- create (w, h)
  ang::(Image GrayScale D32) <- create (w, h)
  withImage re $ \re_ptr ->
    withImage im $ \im_ptr ->
      withImage mag $ \mag_ptr ->
        withImage ang $ \ang_ptr ->
          c'cvCartToPolar (castPtr re_ptr) (castPtr im_ptr) (castPtr mag_ptr) (castPtr ang_ptr) (fromIntegral 0)
  return (mag,ang)
  where
    (re,im) = dftSplit d
    (w,h) = getSize d

dftFromPolar :: (Image GrayScale D32, Image GrayScale D32) -> Image DFT D32
dftFromPolar (mag,ang) = unsafePerformIO $ do
  re::(Image GrayScale D32) <- create (w, h)
  im::(Image GrayScale D32) <- create (w, h)
  withImage mag $ \mag_ptr ->
    withImage ang $ \ang_ptr ->
      withImage re $ \re_ptr -> do
        withImage im $ \im_ptr -> do
          c'cvPolarToCart (castPtr mag_ptr) (castPtr ang_ptr) (castPtr re_ptr) (castPtr im_ptr) (fromIntegral 0)
  return $ dftMerge (re,im)
  where
    (w,h) = getSize mag

--withPolar :: Image DFT D32 ->
--    ((Image GrayScale D32, Image GrayScale D32) -> (Image GrayScale D32, Image GrayScale D32)) ->
--    Image DFT D32


rgbSplit :: Image RGB D32 -> (Image GrayScale D32, Image GrayScale D32, Image GrayScale D32)
rgbSplit i = unsafePerformIO $ do
  r::(Image GrayScale D32) <- create (w, h)
  g::(Image GrayScale D32) <- create (w, h)
  b::(Image GrayScale D32) <- create (w, h)
  withImage i $ \i_ptr ->
    withImage r $ \r_ptr ->
      withImage g $ \g_ptr ->
        withImage b $ \b_ptr -> do
          c'cvSplit (castPtr i_ptr) (castPtr r_ptr) (castPtr g_ptr) (castPtr b_ptr) nullPtr
          return (r,g,b)
  where
    (w,h) = getSize i

rgbMerge :: (Image GrayScale D32, Image GrayScale D32, Image GrayScale D32) -> Image RGB D32
rgbMerge (r,g,b) = unsafePerformIO $ do
  i::(Image RGB D32) <- create (w, h)
  withImage r $ \r_ptr ->
    withImage g $ \g_ptr ->
      withImage b $ \b_ptr ->
        withImage i $ \i_ptr -> do
          c'cvMerge (castPtr r_ptr) (castPtr g_ptr) (castPtr b_ptr) nullPtr (castPtr i_ptr)
          return i
  where
    (w,h) = getSize r
