module CV.Operations
( clear
, set
, NormType(..)
, normalize
, unitNormalize
, unitStretch
, logNormalize
) where

import CV.Bindings.Core
import CV.Bindings.Types
import CV.Image
import CV.ImageMath as IM
import CV.ImageMathOp
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr,castPtr)
import System.IO.Unsafe

clear :: Image c d -> Image c d
clear i = unsafePerformIO $ do
  withImage i $ \i_ptr ->
    c'cvSetZero (castPtr i_ptr)
  return i

set :: Double -> Image c d -> Image c d
set v i = unsafePerformIO $ do
  withImage i $ \i_ptr ->
    c'wrapSetAll (castPtr i_ptr) (realToFrac v) nullPtr
  return i

data NormType =
  NormC |
  NormL1 |
  NormL2 |
  NormMask |
  NormRelative |
  NormDiff |
  NormMinMax |
  NormDiffC |
  NormDiffL1 |
  NormDiffL2 |
  NormDiffRelativeC |
  NormDiffRelativeL1 |
  NormDiffRelativeL2

cNormType t =
  case t of
    NormC              -> c'CV_C
    NormL1             -> c'CV_L1
    NormL2             -> c'CV_L2
    NormMask           -> c'CV_NORM_MASK
    NormRelative       -> c'CV_RELATIVE
    NormDiff           -> c'CV_DIFF
    NormMinMax         -> c'CV_MINMAX
    NormDiffC          -> c'CV_DIFF_C
    NormDiffL1         -> c'CV_DIFF_L1
    NormDiffL2         -> c'CV_DIFF_L2
    NormDiffRelativeC  -> c'CV_RELATIVE_C
    NormDiffRelativeL1 -> c'CV_RELATIVE_L1
    NormDiffRelativeL2 -> c'CV_RELATIVE_L2

normalize :: Double -> Double -> NormType -> Image c d -> Image c d
normalize a b t src =
  unsafePerformIO $ do
    withCloneValue src $ \clone ->
      withImage src $ \si ->
        withImage clone $ \ci -> do
          c'cvNormalize (castPtr si) (castPtr ci) (realToFrac a) (realToFrac b) (cNormType t) nullPtr
          return clone

unitNormalize i
  | minval >= 0 && minval <= 1 && maxval >= 0 && maxval <= 1 = i
  | otherwise = normalize 0 1 NormMinMax i
  where
    m@(minval, maxval) = IM.imageMinMax i

unitStretch i = normalize 0 1 NormMinMax i

logNormalize = unitNormalize . IM.log . (1 |+)
