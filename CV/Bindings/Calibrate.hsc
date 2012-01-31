{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

module CV.Bindings.Calibrate where
import Data.Word
import Foreign.C.Types
import CV.Bindings.Matrix
import CV.Image
import CV.Bindings.Types

#strict_import

#ccall wrapCalibrateCamera2 , Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvSize> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> ->  Ptr <CvMat> -> CInt -> IO Double

#ccall wrapFindCornerSubPix , Ptr BareImage -> Ptr <CvPoint2D32f> -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> IO ()

#num CV_TERMCRIT_ITER
#num CV_TERMCRIT_EPS


#num CV_CALIB_USE_INTRINSIC_GUESS  
#num CV_CALIB_FIX_ASPECT_RATIO     
#num CV_CALIB_FIX_PRINCIPAL_POINT  
#num CV_CALIB_ZERO_TANGENT_DIST    
#num CV_CALIB_FIX_FOCAL_LENGTH 
#num CV_CALIB_FIX_K1  
#num CV_CALIB_FIX_K2  
#num CV_CALIB_FIX_K3  
#num CV_CALIB_FIX_K4  
#num CV_CALIB_FIX_K5  
#num CV_CALIB_FIX_K6  
#num CV_CALIB_RATIONAL_MODEL 


