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
