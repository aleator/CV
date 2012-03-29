{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module CV.Bindings.ImgProc where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import CV.Bindings.Types
import CV.Bindings.Core
import CV.Image
import System.IO.Unsafe

import CV.Bindings.Matrix
#strict_import

#include <bindings.dsl.h>
#include <opencv2/imgproc/imgproc_c.h>
#include "cvWrapLEO.h"
#include "wrapImgProc.h"

-- CVAPI(void) cvCopyMakeBorder(
--   const CvArr* src,
--   CvArr* dst,
--   CvPoint offset,
--   int bordertype,
--   CvScalar value CV_DEFAULT(cvScalarAll(0))
-- );

-- Copies source 2D array inside of the larger destination array and
-- makes a border of the specified type (IPL_BORDER_*) around the copied area.

#ccall wrapCopyMakeBorder , Ptr <CvArr> -> CInt -> CInt -> CInt -> CInt -> CInt -> CFloat -> IO (Ptr BareImage)

data BorderType = BorderConstant | BorderReplicate | BorderReflect | BorderWrap

cBorderType t = case t of
  BorderConstant -> c'IPL_BORDER_CONSTANT
  BorderReplicate -> c'IPL_BORDER_REPLICATE
  BorderReflect -> c'IPL_BORDER_REFLECT
  BorderWrap -> c'IPL_BORDER_WRAP

copyMakeBorder :: Image d c -> Int -> Int -> Int -> Int -> BorderType -> Float -> IO (Image d c)
copyMakeBorder i t b l r border value =
  withGenImage i $ \iptr ->
      creatingImage $
        c'wrapCopyMakeBorder iptr
            (fromIntegral t)
            (fromIntegral b)
            (fromIntegral l)
            (fromIntegral r)
            (cBorderType border)
            (realToFrac value)

-- CVAPI(void) cvCornerHarris(
--   const CvArr* image,
--   CvArr* harris_responce,
--   int block_size,
--   int aperture_size CV_DEFAULT(3),
--   double k CV_DEFAULT(0.04)
-- );

#ccall cvCornerHarris , Ptr <CvArr> -> Ptr <CvArr> -> Int -> Int -> Double -> IO ()

-- CVAPI(CvSeq*) cvHoughLines2(
--   CvArr* image,
--   void* line_storage,
--   int method,
--   double rho,
--   double theta,
--   int threshold,
--   double param1 CV_DEFAULT(0),
--   double param2 CV_DEFAULT(0)
-- );

#num CV_HOUGH_STANDARD
#num CV_HOUGH_PROBABILISTIC
#num CV_HOUGH_MULTI_SCALE
#num CV_HOUGH_GRADIENT

#ccall cvHoughLines2, Ptr <CvArr> -> Ptr () -> Int -> Double -> Double -> Int -> Double -> Double -> IO ()

#ccall wrapFilter2, Ptr  <CvArr> -> Ptr <CvArr> -> Ptr <CvMat> -> Ptr <CvPoint> -> IO ()

#ccall cvCalcArrBackProject, Ptr (Ptr <IplImage>) -> Ptr <CvArr> -> Ptr <CvHistogram> -> IO ()

#num CV_HIST_ARRAY

#ccall cvCreateHist, Int -> Ptr Int -> Int -> Ptr (Ptr Float) -> Int -> IO (Ptr <CvHistogram>)
#ccall cvReleaseHist, Ptr (Ptr <CvHistogram>) -> IO ()
#ccall cvCalcArrHist, Ptr (Ptr <IplImage>) -> Ptr <CvHistogram> -> Int -> Ptr <CvArr> -> IO ()

newtype Histogram = Histogram (ForeignPtr C'CvHistogram)
creatingHistogram fun = do
    iptr :: Ptr C'CvHistogram <- fun
    fptr :: ForeignPtr C'CvHistogram <- newForeignPtr iptr (with iptr c'cvReleaseHist)
    return . Histogram $ fptr


emptyUniformHistogramND dims =
    withArray dims $ \c_sizes ->
    c'cvCreateHist 1 c_sizes c'CV_HIST_ARRAY nullPtr 1

-- thresholding types

-- | value = value > threshold ? max_value : 0 
#num CV_THRESH_BINARY
-- | value = value > threshold ? 0 : max_value
#num CV_THRESH_BINARY_INV
-- | value = value > threshold ? threshold : value
#num CV_THRESH_TRUNC
-- | value = value > threshold ? value : 0
#num CV_THRESH_TOZERO
-- | value = value > threshold ? 0 : value
#num CV_THRESH_TOZERO_INV
#num CV_THRESH_MASK
-- | Use Otsu algorithm to choose the optimal threshold value;
--   combine the flag with one of the above CV_THRESH_* values.
--   Note: when using this, the threshold value is ignored.
#num CV_THRESH_OTSU

-- | CV_THRESH_OTSU | CV_THRESH_BINARY
#num CV_THRESH_OTSU_BINARY
-- | CV_THRESH_OTSU | CV_THRESH_BINARY_INV
#num CV_THRESH_OTSU_BINARY_INV
-- | CV_THRESH_OTSU | CV_THRESH_TRUNC
#num CV_THRESH_OTSU_TRUNC
-- | CV_THRESH_OTSU | CV_THRESH_TOZERO
#num CV_THRESH_OTSU_TOZERO
-- | CV_THRESH_OTSU | CV_THRESH_TOZERO
#num CV_THRESH_OTSU_TOZERO_INV

-- | Applies fixed-level threshold to grayscale image.
--   This is a basic operation applied before retrieving contours.
--   @
--   CVAPI(double) cvThreshold(
--   const CvArr* src, 
--   CvArr* dst,
--   double threshold,
--   double  max_value,
--   int threshold_type);@

#ccall cvThreshold , Ptr <CvArr> -> Ptr <CvArr> -> CDouble -> CDouble -> CInt -> IO (CDouble)

-- | Threshold for each pixel is the mean calculated from /block_size/
--   neighborhood, minus /param1/.
#num CV_ADAPTIVE_THRESH_MEAN_C
-- | Threshold for each pixel is the gaussian mean calculated from /block_size/
--   neighborhood, minus /param1/
#num CV_ADAPTIVE_THRESH_GAUSSIAN_C

-- | Applies adaptive threshold to grayscale image.
--   The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
--   CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
--   neighborhood size (3, 5, 7 etc.),
--   and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...)
--   @
--   CVAPI(void) cvAdaptiveThreshold(
--   const CvArr* src,
--   CvArr* dst,
--   double max_value,
--   int adaptive_method CV_DEFAULT(CV_ADAPTIVE_THRESH_MEAN_C),
--   int threshold_type CV_DEFAULT(CV_THRESH_BINARY),
--   int block_size CV_DEFAULT(3),
--   double param1 CV_DEFAULT(5));@

#ccall cvAdaptiveThreshold , Ptr <CvArr> -> Ptr <CvArr> -> CDouble -> CInt -> CInt -> CInt -> CDouble -> IO ()

-- | Fills the connected component until the color difference gets large enough
--   @
--   CVAPI(void) cvFloodFill(
--   CvArr* image,
--   CvPoint seed_point,
--   CvScalar new_val,
--   CvScalar lo_diff CV_DEFAULT(cvScalarAll(0)),
--   CvScalar up_diff CV_DEFAULT(cvScalarAll(0)),
--   CvConnectedComp* comp CV_DEFAULT(NULL),
--   int flags CV_DEFAULT(4),
--   CvArr* mask CV_DEFAULT(NULL));@
