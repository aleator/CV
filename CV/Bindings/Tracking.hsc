{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include "cvWrapLEO.h"
#include <opencv2/legacy/legacy.hpp>

module CV.Bindings.Tracking where
import Data.Word
import Foreign.C.Types
import CV.Bindings.Matrix
import CV.Image
import CV.Bindings.Types

#strict_import

#ccall wrapCamShift, Ptr <CvArr> -> Ptr <CvRect> -> Ptr <CvTermCriteria>-> Ptr <CvConnectedComp> -> Ptr <CvBox2D> -> IO ()

#ccall wrapMeanShift, Ptr <CvArr> -> Ptr <CvRect> -> Ptr <CvTermCriteria> -> Ptr <CvConnectedComp> -> IO ()

-- void wrapSnakeImage(const IplImage* image, CvPoint* points, int length, float* alpha, float* beta, float* gamma, int coeff_usage, CvSize win, CvTermCriteria criteria, int calc_gradient=1)¶

#ccall cvSnakeImage, Ptr <IplImage> -> Ptr <CvPoint> -> Int -> Ptr Float -> Ptr Float -> Ptr Float -> Int -> Ptr <CvSize> -> Ptr <CvTermCriteria> -> Int -> IO ()

#num CV_VALUE
#num CV_ARRAY

