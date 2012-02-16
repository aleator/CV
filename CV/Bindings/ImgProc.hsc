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
