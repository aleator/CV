{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Features where

import Foreign.C.Types
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include <opencv/cv.h>

#starttype CvSURFParams
#field extended, CInt
#field hessianThreshold, CDouble
#field nOctaves, CInt
#field nOctaveLayers, CInt
#stoptype

#ccall wrapExtractSURF, Ptr <CvArr> -> Ptr <CvArr> -> Ptr (Ptr <CvSeq>) -> Ptr (Ptr <CvSeq>) -> Ptr <CvMemStorage> -> Ptr <CvSURFParams> -> Int -> IO ()

#starttype CvMSERParams
#field delta, Int
#field maxArea, Int
#field minArea, Int
#field maxVariation, Float
#field minDiversity, Float
#field maxEvolution, Int
#field areaThreshold, Double
#field minMargin, Double
#field edgeBlurSize, Int
#stoptype

#ccall wrapExtractMSER, Ptr <CvArr> -> Ptr <CvArr> -> Ptr (Ptr <CvSeq>) -> Ptr <CvMemStorage> -> Ptr <CvMSERParams> -> IO ()
