 {-#LANGUAGE TypeSynonymInstances#-}
 -- | Image thresholding operations
module CV.Thresholding(
  -- * Interfaces to OpenCV functions
  ThresholdType(..)
, threshold
, thresholdInPlace
, thresholdOtsu
, AdaptiveType(..)
, adaptiveThreshold
  -- * Other methods
, bernsen
, nibbly
, nibblyr
, kittler
, kittlerMeasure
, betweenClassVariance
) where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Morphology
import System.IO.Unsafe
import CV.Sampling
import Utils.List
import Unsafe.Coerce
import Data.List
import CV.Histogram
import CV.Bindings.ImgProc
import Foreign.Ptr (nullPtr,castPtr)

-- | A type class for selecting the maximum value for each image type, used in
--   creating the thresholded image
class MaxVal a where
  maxval :: (Image GrayScale a) -> Double

instance MaxVal D32 where
  maxval _ = 1

instance MaxVal D8 where
  maxval _ = 255

-- | Thresholding behavior for values larger and smaller than threshold
data ThresholdType
  -- | Values larger than threshold are set to max, smaller to zero
  = MaxAndZero
  -- | Values larger than threshold are set to zero, smaller to max
  | ZeroAndMax
  -- | Values larger than threshold are truncated to threshold, smaller are not touched
  | ThreshAndValue
  -- | Values larger than threshold are not touched, smaller are set to zero
  | ValueAndZero
  -- | Values larger than threshold are set to zero, smaller are not touched
  | ZeroAndValue

-- | Utility function for converting ThresholdType to c values
cThresholdType t =
  case t of
    MaxAndZero     -> c'CV_THRESH_BINARY
    ZeroAndMax     -> c'CV_THRESH_BINARY_INV
    ThreshAndValue -> c'CV_THRESH_TRUNC
    ValueAndZero   -> c'CV_THRESH_TOZERO
    ZeroAndValue   -> c'CV_THRESH_TOZERO_INV

-- | Utility function for converting otsu ThresholdType to c values
cOtsuThresholdType t =
  case t of
    MaxAndZero     -> c'CV_THRESH_OTSU_BINARY
    ZeroAndMax     -> c'CV_THRESH_OTSU_BINARY_INV
    ThreshAndValue -> c'CV_THRESH_OTSU_TRUNC
    ValueAndZero   -> c'CV_THRESH_OTSU_TOZERO
    ZeroAndValue   -> c'CV_THRESH_OTSU_TOZERO_INV

-- | Method used for selecting the adaptive threshold value
data AdaptiveType
  -- | Threshold using the arithmetic mean of pixel neighborhood
  = ByMean
  -- | Threshold using the gaussian weighted mean of pixel neighborhood
  | ByGaussian 

-- | Utility function for converting AdaptiveType to c value
cAdaptiveType t =
  case t of
    ByMean     -> c'CV_ADAPTIVE_THRESH_MEAN_C
    ByGaussian -> c'CV_ADAPTIVE_THRESH_GAUSSIAN_C

-- | Thresholds a grayscale image according to the selected type, using the
--   given threshold value.
threshold :: (MaxVal d) => ThresholdType -> Double -> Image GrayScale d -> Image GrayScale d
threshold ttype tval image =
  unsafePerformIO $
    withCloneValue image $ \result ->
      withImage image $ \pimage ->
        withImage result $ \presult -> do
          c'cvThreshold (castPtr pimage) (castPtr presult) (realToFrac tval)
            (realToFrac (maxval image)) (cThresholdType ttype)
          return result

-- TODO: Convert into imageOperation
thresholdInPlace :: (MaxVal d) => ThresholdType -> Double -> Image GrayScale d -> IO (Image GrayScale D8)
thresholdInPlace ttype tval image = do
      withImage image $ \pimage ->
          c'cvThreshold (castPtr pimage) (castPtr pimage) (realToFrac tval)
            (realToFrac (maxval image)) (cThresholdType ttype)
      return (unsafeCoerce image)

-- | Thresholds a grayscale image using the otsu method according to the
--   selected type. Threshold value is selected automatically, and only 8-bit
--   images are supported.
thresholdOtsu :: ThresholdType -> Image GrayScale D8 -> Image GrayScale D8
thresholdOtsu ttype image =
  unsafePerformIO $
    withCloneValue image $ \result ->
      withImage image $ \pimage ->
        withImage result $ \presult -> do
          c'cvThreshold (castPtr pimage) (castPtr presult) 0
            (realToFrac 255) (cOtsuThresholdType ttype)
          return result

-- | Applies adaptive thresholding by selecting the optimal threshold value for
--   each pixel. The threshold is selected by calculating the arithmetic or
--   gaussian-weighted mean of a pixel neighborhood, and applying a bias term to
--   the obtained value.
adaptiveThreshold :: (MaxVal d) => AdaptiveType -> ThresholdType -> Int -> Double 
  -> Image GrayScale d -> Image GrayScale d
adaptiveThreshold a t neighborhood bias image =
  unsafePerformIO $
    withCloneValue image $ \result ->
      withImage image $ \pimage ->
        withImage result $ \presult -> do
          c'cvAdaptiveThreshold (castPtr pimage) (castPtr presult) (realToFrac (maxval image))
            (cAdaptiveType a) (cThresholdType t) (fromIntegral neighborhood) (realToFrac bias)
          return result

bernsen (w,h) c i = goodContrast #* (i #< surface)
            where
             low  = erode se 1  i
             high = dilate se 1 i
             goodContrast = IM.moreThan c (high #- low)
             surface = 0.5 |* (high #+ low) 
             se = structuringElement (w,h) (w`div`2,h`div`2) EllipseShape

-- Very slow implementation of niblack thresholding
--niblack (w,h) k i = IM.more2Than trunc (unsafePerformIO $ surface) 
--    where
--     trunc = getRegion (w`div`2,h`div`2) (wi-w,hi-h) i
--     (wi,hi) = getSize i
--     surface = renderFlatList (wi-w,hi-h) (map th patches)
--     th ptch = IM.average ptch + k * IM.stdDeviation ptch
--     patches = allPatches (w,h) i

nibbly k c i = let dev = IM.stdDeviation i
                   mean = IM.average i 
             in IM.moreThan (mean+k*dev+c) i

nibblyr (w,h) k i = IM.lessThan t flat
    where
     t = IM.average flat + k * IM.stdDeviation flat
     flat = i #- gaussian (w,h) i


---- TODO: Convert Histograms from Doubles to Floats..
--otsu bs image = IM.moreThan (realToFrac threshold) image
--    where
--        histogram  = getHistogram bs $ image
--        partitions = histogramPartitions histogram 
--        (threshold,_,_) = maximumBy (comparing otsuCmp) partitions 
--        otsuCmp (t,as,bs) = betweenClassVariance (as) (bs)

-- This is excruciatingly slow means of finding kittler-illingworth threshold
-- for an image
kittler precision image = IM.moreThan t image
    where t = maximumBy (comparing (kittlerMeasure image))
                            [0,0+precision..1]

kittlerMeasure image t = unNaN $ 
                        p_t*log fgDev
                      + (1-p_t)*log bgDev
                      - p_t*log p_t 
                      - (1-p_t)*log(1-p_t)
    where
     unNaN x | isNaN x = -10000000
             | otherwise = x
     thresholded = unsafeImageTo32F (IM.lessThan t image)
     p_t = IM.sum ( thresholded) / fromIntegral (getArea image)
     bgDev = realToFrac $ IM.stdDeviationMask image thresholded 
     fgDev = realToFrac $ IM.stdDeviationMask image (IM.invert thresholded)


--histogramPartitions (HGD a) = zip3 (head.tails.map fst $ a) 
--                                   (tail.inits.map snd $ a) 
--                                   (reverse.tail.reverse.tails.map snd $ a)

betweenClassVariance as bs = sum as * sum bs 
                             * (average bs - average as)^2

