{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.ImgProc where

import Foreign.C.Types
import Foreign.Ptr
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include <opencv/cv.h>

-- CVAPI(void) cvCornerHarris(
--   const CvArr* image,
--   CvArr* harris_responce,
--   int block_size,
--   int aperture_size CV_DEFAULT(3),
--   double k CV_DEFAULT(0.04)
-- );

#ccall cvCornerHarris , Ptr <CvArr> -> Ptr <CvArr> -> Int -> Int -> Double -> IO ()

-- CVAPI(CvSeq*)  cvHoughLines2( CvArr* image, void* line_storage, int method,
--                               double rho, double theta, int threshold,
--                               double param1 CV_DEFAULT(0), double param2 CV_DEFA
-- ULT(0));

#num CV_HOUGH_STANDARD
#num CV_HOUGH_PROBABILISTIC
#num CV_HOUGH_MULTI_SCALE
#num CV_HOUGH_GRADIENT


#ccall cvHoughLines2, Ptr <CvArr> -> Ptr () -> Int -> Double -> Double -> Int -> Double -> Double -> IO ()

