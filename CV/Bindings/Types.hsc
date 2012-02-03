{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Types where
import Data.Word
import Foreign.C.Types
import Foreign.Storable

#strict_import

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

#opaque_t CvArr

#starttype CvSize
#field width , CInt
#field height , CInt
#stoptype

#starttype CvSize2D32f
#field width , CFloat
#field height , CFloat
#stoptype

#starttype CvPoint2D32f
#field x , Float
#field y , Float
#stoptype

-- // #starttype CV_32FC2
-- // #field x , Float
-- // #field y , Float
-- // #stoptype

mkCvPoint2D32F (x,y) = C'CvPoint2D32f x y


#starttype CvBox2D
#field center ,<CvPoint2D32f>
#field size   ,<CvSize2D32f>
#field angle  ,CFloat
#stoptype

