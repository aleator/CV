{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Types where
import Data.Word
import Foreign.C.Types
import Foreign.Storable

#strict_import

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

#opaque_t CvArr

#starttype CvScalar
#field val[0] , CDouble
#field val[1] , CDouble
#field val[2] , CDouble
#field val[3] , CDouble
#stoptype

#starttype CvSize
#field width , CInt
#field height , CInt
#stoptype

#starttype CvSize2D32f
#field width , CFloat
#field height , CFloat
#stoptype

#starttype CvPoint
#field x , CInt
#field y , CInt
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

#num CV_8UC1 
#num CV_8UC2 
#num CV_8UC3 
#num CV_8UC4 

#num CV_8SC1 
#num CV_8SC2 
#num CV_8SC3 
#num CV_8SC4 

#num CV_16UC1 
#num CV_16UC2 
#num CV_16UC3 
#num CV_16UC4 

#num CV_16SC1 
#num CV_16SC2 
#num CV_16SC3 
#num CV_16SC4 

#num CV_32SC1 
#num CV_32SC2 
#num CV_32SC3 
#num CV_32SC4 

#num CV_32FC1 
#num CV_32FC2 
#num CV_32FC3 
#num CV_32FC4 

#num CV_64FC1 
#num CV_64FC2 
#num CV_64FC3 
#num CV_64FC4 
