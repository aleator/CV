{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Core where

import Foreign.C.Types
import CV.Bindings.Types
import CV.Image(BareImage)

#strict_import

#include <bindings.dsl.h>
-- #include <opencv/cv.h>
#include "cvWrapCore.h"
#include <opencv2/core/core_c.h>

-- CVAPI(IplImage*) cvCreateImage(
--   CvSize size,
--   int depth,
--   int channels
-- );

-- Creates IPL image (header and data)

-- #ccall wrapCreateImage , CInt -> CInt -> CInt -> CInt -> IO (Ptr IplImage)

-- Releases IPL image header and data
-- CVAPI(void) cvReleaseImage(
--   IplImage** image
-- );

-- Allocates and initializes CvMat header and allocates data
-- CVAPI(CvMat*) cvCreateMat(
--  int rows,
--  int cols,
--  int type
-- );

-- Releases CvMat header and deallocates matrix data
-- (reference counting is used for data)
-- CVAPI(void) cvReleaseMat(
--   CvMat** mat
-- );

-- CVAPI(void) cvSetZero(
--   CvArr* arr
-- );

-- Clears all the array elements (sets them to 0)

#ccall cvSetZero , Ptr <CvArr> -> IO ()

-- CVAPI(void) cvSplit(
--   const CvArr* src,
--   CvArr* dst0,
--   CvArr* dst1,
--   CvArr* dst2,
--   CvArr* dst3
-- );

-- Splits a multi-channel array into the set of single-channel arrays or
-- extracts particular [color] plane

#ccall cvSplit , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> IO ()

-- CVAPI(void) cvMerge(
--   const CvArr* src0,
--   const CvArr* src1,
--   const CvArr* src2,
--   const CvArr* src3,
--   CvArr* dst
-- );

-- Merges a set of single-channel arrays into the single multi-channel array
-- or inserts one particular [color] plane to the array

#ccall cvMerge , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> IO ()

-- CVAPI(void) cvCartToPolar(
--   const CvArr* x,
--   const CvArr* y,
--   CvArr* magnitude,
--   CvArr* angle CV_DEFAULT(NULL),
--   int angle_in_degrees CV_DEFAULT(0)
-- );

-- Does cartesian->polar coordinates conversion.
-- Either of output components (magnitude or angle) is optional

#ccall cvCartToPolar , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> CInt -> IO ()

-- CVAPI(void) cvPolarToCart(
--   const CvArr* magnitude,
--   const CvArr* angle,
--   CvArr* x,
--   CvArr* y,
--   int angle_in_degrees CV_DEFAULT(0)
-- );

-- Does polar->cartesian coordinates conversion.
-- Either of output components (magnitude or angle) is optional.
-- If magnitude is missing it is assumed to be all 1's

#ccall cvPolarToCart , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> CInt -> IO ()

-- CVAPI(void) cvAvgSdv(
--   const CvArr* arr,
--   CvScalar* mean,
--   CvScalar* std_dev,
--   const CvArr* mask CV_DEFAULT(NULL)
-- );

-- #ccall cvAvgSdv , Ptr BareImage -> Ptr <CvScalar> -> Ptr <CvScalar> -> Ptr <CvArr> -> IO ()

-- types of array norm

#num CV_C
#num CV_L1
#num CV_L2
#num CV_NORM_MASK
#num CV_RELATIVE
#num CV_DIFF
#num CV_MINMAX

#num CV_DIFF_C
#num CV_DIFF_L1
#num CV_DIFF_L2
#num CV_RELATIVE_C
#num CV_RELATIVE_L1
#num CV_RELATIVE_L2

-- CVAPI(void) cvNormalize(
--   const CvArr* src,
--   CvArr* dst,
--   double a CV_DEFAULT(1.),
--   double b CV_DEFAULT(0.),
--   int norm_type CV_DEFAULT(CV_L2),
--   const CvArr* mask CV_DEFAULT(NULL)
-- );

#ccall cvNormalize , Ptr <CvArr> -> Ptr <CvArr> -> CDouble -> CDouble -> CInt -> Ptr <CvArr> -> IO ()

-- stuff related to DFT

#num CV_DXT_FORWARD
#num CV_DXT_INVERSE
#num CV_DXT_SCALE
#num CV_DXT_INV_SCALE
#num CV_DXT_ROWS
#num CV_DXT_MUL_CONJ
#num CV_DXT_COMPLEX_OUTPUT
#num CV_DXT_REAL_OUTPUT
#num CV_DXT_INV_REAL

-- CVAPI(void) cvDFT(
--   const CvArr* src,
--   CvArr* dst,
--   int flags,
--   int nonzero_rows CV_DEFAULT(0)
-- );

-- Discrete Fourier Transform:
--   complex->complex,
--   real->ccs (forward),
--   ccs->real (inverse)

#ccall cvDFT , Ptr <CvArr> -> Ptr <CvArr> -> CInt -> CInt -> IO ()

-- #ccall cvWrapDFT , Ptr <CvArr> -> Ptr <CvArr> -> IO ()

-- #ccall cvWrapIDFT , Ptr <CvArr> -> Ptr <CvArr> -> IO ()

-- CVAPI(void) cvMulSpectrums(
--   const CvArr* src1,
--   const CvArr* src2,
--   CvArr* dst,
--   int flags
-- );

#ccall swapQuadrants, Ptr <CvArr> -> IO ()

-- Multiply results of DFTs: DFT(X)*DFT(Y) or DFT(X)*conj(DFT(Y))

#ccall cvMulSpectrums , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> CInt -> IO ()

-- CVAPI(int) cvGetOptimalDFTSize(
--   int size0
-- );

-- Finds optimal DFT vector size >= size0
-- Note: this function has no side effects, thus not called in IO monad.

#ccall cvGetOptimalDFTSize , CInt -> CInt

-- CVAPI(void) cvDCT(
--   const CvArr* src
--   CvArr* dst,
--   int flags
-- );

-- Discrete Cosine Transform

#ccall cvDCT , Ptr <CvArr> -> Ptr <CvArr> -> CInt -> IO ()
