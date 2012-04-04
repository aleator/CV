{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Error where

import Foreign.C.Types
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include "cvWrapCore.h"
#include <opencv2/core/core_c.h>

#callback CvErrorCallback,  CInt -> Ptr CChar -> Ptr CChar -> Ptr CChar -> CInt -> IO CInt
#ccall cvRedirectError, <CvErrorCallback> -> Ptr () -> Ptr (Ptr ()) -> IO ()
#ccall cvSetErrMode, CInt -> IO CInt

#num CV_ErrModeLeaf
#num CV_ErrModeParent
#num CV_ErrModeSilent 
