{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Projection where

import Foreign.Ptr
import CV.Bindings.Types
import CV.Image(BareImage)
import CV.Bindings.Matrix

#strict_import

#include <bindings.dsl.h>
#include "cvProjection.h"

#ccall project_polar , Ptr BareImage -> IO (Ptr BareImage)
#ccall cvComputeCorrespondEpilines , Ptr <CvMat> -> CInt -> Ptr <CvMat> -> Ptr <CvMat> -> IO ()

#ccall cvProjectPoints2, Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> CDouble -> IO ()

#ccall cvTriangulatePoints, Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> IO ()
