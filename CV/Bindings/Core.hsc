{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Core where

import Foreign.C.Types
import CV.Bindings.Types
import CV.Image(BareImage)

#strict_import

#include <bindings.dsl.h>
#include "cvWrapCore.h"
#include <opencv2/core/core_c.h>

#ccall wrapSet , Ptr <CvArr> -> Ptr <CvScalar> -> Ptr <CvArr> -> IO ()
#ccall wrapSetAll , Ptr <CvArr> -> CDouble -> Ptr <CvArr> -> IO ()
#ccall wrapSet1 , Ptr <CvArr> -> CDouble -> Ptr <CvArr> -> IO ()
#ccall wrapSet2 , Ptr <CvArr> -> CDouble -> CDouble -> Ptr <CvArr> -> IO ()
#ccall wrapSet3 , Ptr <CvArr> -> CDouble -> CDouble -> CDouble -> Ptr <CvArr> -> IO ()
#ccall wrapSet4 , Ptr <CvArr> -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr <CvArr> -> IO ()

-- Clears all the array elements (sets them to 0)
#ccall cvSetZero , Ptr BareImage -> IO ()

#ccall cvSplit , Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> IO ()

#ccall cvMerge , Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> IO ()

#ccall cvCartToPolar , Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> CInt -> IO ()

-- | CVAPI(void) cvPolarToCart(
-- |   const CvArr* magnitude,
-- |   const CvArr* angle,
-- |   CvArr* x,
-- |   CvArr* y,
-- |   int angle_in_degrees CV_DEFAULT(0)
-- | );

-- | Does polar->cartesian coordinates conversion.
--   Either of output components (magnitude or angle) is optional.
--   If magnitude is missing it is assumed to be all 1's
#ccall cvPolarToCart , Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> Ptr <CvArr> -> CInt -> IO ()

-- | CVAPI(void) cvMinMaxLoc(
-- |   const CvArr* arr,
-- |   double* min_val,
-- |   double* max_val,
-- |   CvPoint* min_loc CV_DEFAULT(NULL),
-- |   CvPoint* max_loc CV_DEFAULT(NULL),
-- |   const CvArr* mask CV_DEFAULT(NULL)
-- | );

-- | Finds global minimum, maximum and their positions
#ccall cvMinMaxLoc , Ptr <CvArr> -> Ptr CDouble -> Ptr CDouble -> Ptr <CvPoint> -> Ptr <CvPoint> -> Ptr <CvArr> -> IO ()

-- | CVAPI(void) cvAvgSdv(
-- |   const CvArr* arr,
-- |   CvScalar* mean,
-- |   CvScalar* std_dev,
-- |   const CvArr* mask CV_DEFAULT(NULL)
-- | );

-- | Calculates mean and standard deviation of pixel values.
#ccall cvAvgSdv , Ptr BareImage -> Ptr <CvScalar> -> Ptr <CvScalar> -> Ptr <CvArr> -> IO ()

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

#ccall cvNormalize , Ptr BareImage -> Ptr BareImage -> CDouble -> CDouble -> CInt -> Ptr BareImage -> IO ()

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
#num CV_DXT_INVERSE_SCALE

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

#ccall cvDFT , Ptr BareImage -> Ptr BareImage -> CInt -> CInt -> IO ()

-- CVAPI(void) cvMulSpectrums(
--   const CvArr* src1,
--   const CvArr* src2,
--   CvArr* dst,
--   int flags
-- );

-- Multiply results of DFTs: DFT(X)*DFT(Y) or DFT(X)*conj(DFT(Y))

#ccall cvMulSpectrums , Ptr BareImage -> Ptr BareImage -> Ptr BareImage -> CInt -> IO ()

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

#ccall cvDCT , Ptr BareImage -> Ptr BareImage -> CInt -> IO ()

-- TODO: This might be expanded:
#opaque_t CvSeq
