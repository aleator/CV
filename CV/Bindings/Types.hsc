{-# LANGUAGE ForeignFunctionInterface, TypeFamilies #-}
module CV.Bindings.Types where

import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Utils.GeometryClass
import GHC.Float

#strict_import

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

#num IPL_DEPTH_1U
#num IPL_DEPTH_8U
#num IPL_DEPTH_16U
#num IPL_DEPTH_32F
#num IPL_DEPTH_8S
#num IPL_DEPTH_16S
#num IPL_DEPTH_32S

#num IPL_BORDER_CONSTANT
#num IPL_BORDER_REPLICATE
#num IPL_BORDER_REFLECT
#num IPL_BORDER_WRAP

#opaque_t IplImage
#opaque_t CvMemStorage
#opaque_t CvSeqBlock
#opaque_t CvArr

#opaque_t CvHistogram

#starttype CvSeq
#field flags, CInt
#field header_size, CInt
#field h_prev, Ptr <CvSeq>
#field h_next, Ptr <CvSeq>
#field v_prev, Ptr <CvSeq>
#field v_next, Ptr <CvSeq>
#field total,  CInt
#field elem_size, CInt
#field block_max, Ptr Char
#field ptr, Ptr Char
#field delta_elems, CInt
#field free_blocks, Ptr <CvSeqBlock>
#field first, Ptr <CvSeqBlock>
#stoptype

#ccall extractCVSeq, Ptr <CvSeq> -> Ptr () -> IO ()
#ccall cvGetSeqElem, Ptr <CvSeq> -> CInt -> IO (Ptr CChar)
#ccall printSeq, Ptr <CvSeq> -> IO ()

-- | Convert a CvSeq object into list of its contents. Note that
-- since CvSeq can be approximately anything, including a crazy man from the moon,
-- this is pretty unsafe and you must make sure that `a` is actually the element
-- in the seq, and the seq is something that remotely represents a sequence of elements.
cvSeqToList :: (Storable a) => Ptr C'CvSeq -> IO [a]
cvSeqToList ptrseq = do
   seq <- peek ptrseq
   dest <- mallocArray (fromIntegral $ c'CvSeq'total seq)
   c'extractCVSeq ptrseq (castPtr dest)
   peekArray (fromIntegral $ c'CvSeq'total seq) dest


#starttype CvRect
#field x , Int
#field y , Int
#field width , Int
#field height , Int
#stoptype

-- CvScalar

-- typedef struct CvScalar
-- {
-- double val[4];
-- }
-- CvScalar;

#starttype CvScalar
#field val[0] , CDouble
#field val[1] , CDouble
#field val[2] , CDouble
#field val[3] , CDouble
#stoptype

-- CV_INLINE CvScalar cvScalar(
--   double val0,
--   double val1 CV_DEFAULT(0),
--   double val2 CV_DEFAULT(0),
--   double val3 CV_DEFAULT(0)
-- )

-- #cinline cvScalar , CDouble -> CDouble -> CDouble -> CDouble -> IO(<CvScalar>)

-- CV_INLINE CvScalar cvRealScalar(
--   double val0
-- )

-- #cinline cvRealScalar , CDouble -> IO(<CvScalar>)

-- CV_INLINE CvScalar cvScalarAll(
--   double val0123
-- )

-- #cinline cvScalarAll , CDouble -> IO(<CvScalar>)

-- CvSize

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

instance Point2D C'CvPoint where
   type ELP C'CvPoint = Int
   pt (C'CvPoint x y) = (fromIntegral x,fromIntegral y)
   toPt (x,y) = C'CvPoint (fromIntegral x) (fromIntegral y)

#starttype CvPoint2D32f
#field x , Float
#field y , Float
#stoptype

instance Point2D C'CvPoint2D32f where
   type ELP C'CvPoint2D32f = Double
   pt (C'CvPoint2D32f x y) = (realToFrac x,realToFrac y)
   toPt (x,y) = C'CvPoint2D32f (realToFrac x) (realToFrac y)

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

-- #startype CvHistogram
-- #field type, Int
-- #field bins, Ptr <CvArr>
-- #array_field thresh, Ptr (Ptr Float)
-- #field thresh2, Ptr (Ptr Float)
-- #array_field mat, <CvMatND>
-- #endtype



#starttype CvTermCriteria
#field type, Int
#field max_iter, Int
#field epsilon, Double
#stoptype

#num CV_TERMCRIT_ITER
#num CV_TERMCRIT_NUMBER
#num CV_TERMCRIT_EPS


-- Memory Storage
#ccall cvCreateMemStorage, Int -> IO (Ptr <CvMemStorage>)
#ccall cvReleaseMemStorage, Ptr (Ptr <CvMemStorage>) -> IO ()

withNewMemory fun = do
    mem <- c'cvCreateMemStorage 0
    res <- fun mem
    with mem c'cvReleaseMemStorage
    return res


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


#starttype CvSURFPoint
#field pt, <CvPoint2D32f>
#field laplacian, CInt
#field size, CInt
#field dir, CFloat
#field hessian, CFloat
#stoptype

instance Point2D C'CvSURFPoint where
   type ELP C'CvSURFPoint = Float
   pt (C'CvSURFPoint (C'CvPoint2D32f x y) _ _ _ _) = (realToFrac x,realToFrac y)
   toPt (x,y) = C'CvSURFPoint (C'CvPoint2D32f (realToFrac x) (realToFrac y)) 0 0 0 0

