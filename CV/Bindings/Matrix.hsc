{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

module CV.Bindings.Matrix where
import Data.Word
import Foreign.C.Types

#strict_import

#starttype CvMat
#field type , CInt 
#field step , CInt
#field refcount , Ptr CInt 
#union_field data.ptr , Ptr CUChar 
#union_field data.s   , Ptr CShort 
#union_field data.i   , Ptr CInt 
#union_field data.fl  , Ptr CFloat 
#union_field data.db  , Ptr CDouble 
#field rows , CInt
#field cols , CInt
#stoptype

#ccall cvCreateMat  , Int -> Int -> Int -> IO (Ptr <CvMat>) 
#ccall cvReleaseMat , Ptr (Ptr <CvMat>) -> IO ()

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


-- typedef struct CvMat
-- {
--     int type;
--     int step;
--     int* refcount;
--     union
--     {
--         uchar* ptr;
--         short* s;
--         int* i;
--         float* fl;
--         double* db;
--     } data;
--     int rows;
--     int cols;
-- } CvMat;


