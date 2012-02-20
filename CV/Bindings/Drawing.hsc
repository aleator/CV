{-# LANGUAGE ForeignFunctionInterface#-}
module CV.Bindings.Drawing where
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Utils.GeometryClass
import GHC.Float
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

#ccall wrapEllipseBox, Ptr <CvArr> -> Ptr <CvBox2D> -> Ptr <CvScalar> -> CInt -> CInt -> CInt -> IO ()
