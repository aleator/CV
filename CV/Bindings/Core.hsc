{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Core where

import Foreign.C.Types
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include <opencv/cv.h>
  -- #include <opencv/cxcore.h>

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

#ccall cvNormalize , Ptr <CvArr> -> Ptr <CvArr> -> Double -> Double -> Int -> Ptr <CvArr> -> IO ()

-- CVAPI(void) cvAvgSdv(
--   const CvArr* arr,
--   CvScalar* mean,
--   CvScalar* std_dev,
--   const CvArr* mask CV_DEFAULT(NULL)
-- );

#ccall cvAvgSdv , Ptr <CvArr> -> Ptr <CvScalar> -> Ptr <CvScalar> -> Ptr <CvArr> -> IO ()
