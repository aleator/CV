{-# LANGUAGE ScopedTypeVariables #-}
module CV.DFT where

import CV.Bindings.Types
import CV.Bindings.Core
import CV.Bindings.ImgProc
import CV.Image
import CV.ImageMath as IM
import CV.ImageMathOp
import System.IO.Unsafe
import Foreign.Ptr

dft :: Image GrayScale d -> Image Complex D32
dft i = unsafePerformIO $ do
  n::(Image GrayScale D32) <- create (w', h')
  n <- copyMakeBorder i 0 (h'-h) 0 (w'-w) BorderConstant 0
  c::(Image Complex D32) <- create (w', h')
  withGenImage n $ \nimg ->
    withGenImage c $ \cimg -> do
      c'cvMerge nimg nullPtr nullPtr nullPtr cimg
      c'cvDFT cimg cimg c'CV_DXT_FORWARD 0
      return c
  where
    (w,h) = getSize i
    w' = fromIntegral $ c'cvGetOptimalDFTSize (fromIntegral w)
    h' = fromIntegral $ c'cvGetOptimalDFTSize (fromIntegral h)

idft :: Image Complex D32 -> Image GrayScale D32
idft c = unsafePerformIO $ do
  n::(Image GrayScale D32) <- create s
  withGenImage c $ \c_ptr ->
    withGenImage n $ \n_ptr -> do
      c'cvDFT c_ptr c_ptr c'CV_DXT_INVERSE 0
      c'cvSplit c_ptr n_ptr nullPtr nullPtr nullPtr
      return n
  where
    s = getSize c

complexSplit :: Image Complex D32 -> (Image GrayScale D32, Image GrayScale D32)
complexSplit c = unsafePerformIO $ do
  re::(Image GrayScale D32) <- create (w, h)
  im::(Image GrayScale D32) <- create (w, h)
  withGenImage c $ \c_ptr ->
    withGenImage re $ \re_ptr ->
      withGenImage im $ \im_ptr -> do
        c'cvSplit c_ptr re_ptr im_ptr nullPtr nullPtr
        return (re,im)
  where
    (w,h) = getSize c

complexToMagnitude :: Image Complex D32 -> Image GrayScale D32
complexToMagnitude c = unsafePerformIO $ do
  mag::(Image GrayScale D32) <- create (w, h)
  withGenImage re $ \re_ptr ->
    withGenImage im $ \im_ptr ->
      withGenImage mag $ \mag_ptr -> do
        c'cvCartToPolar re_ptr im_ptr mag_ptr nullPtr (fromIntegral 0)
        return $ IM.log $ 1 |+ mag
  where
    (re,im) = complexSplit c
    (w,h) = getSize c
