module CV.Operations
( NormType(..)
, normalize
, unitNormalize
, logNormalize
) where

import CV.Bindings.Types
import CV.Bindings.Core
import CV.Image
import CV.ImageMath as IM
import CV.ImageMathOp
import C2HSTools

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
      withGenImage src $ \si ->
        withGenImage clone $ \ci -> do
          c'cvNormalize si ci (realToFrac a) (realToFrac b) (cNormType t) nullPtr
          return clone

unitNormalize = normalize 0 1 NormMinMax

logNormalize = unitNormalize . IM.log . (1 |+)
