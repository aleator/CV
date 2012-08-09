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

#ccall cvTranspose  , Ptr <CvMat> -> Ptr <CvMat> -> IO ()
#ccall cvInvert     , Ptr <CvMat> -> Ptr <CvMat> -> CInt -> IO ()
#ccall cvGEMM       , Ptr <CvMat> -> Ptr <CvMat> -> Double -> Ptr <CvMat> -> Double -> Ptr <CvMat> -> Int -> IO ()

#ccall cvRodrigues2  , Ptr <CvMat> -> Ptr <CvMat> -> Ptr <CvMat> -> IO Int

-- Matrix inversions
#num CV_LU 
#num CV_SVD 
#num CV_SVD_SYM 

#num CV_GEMM_A_T
#num CV_GEMM_B_T
#num CV_GEMM_C_T



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


