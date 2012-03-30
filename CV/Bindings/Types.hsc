{-# LANGUAGE ForeignFunctionInterface, TypeFamilies #-}
module CV.Bindings.Types where

import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Utils.GeometryClass
import Utils.Rectangle
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

#starttype CvContour
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
#field rect, <CvRect>
#field color, CInt
#field reserved[0], CInt
#field reserved[1], CInt
#field reserved[2], CInt
#stoptype

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
#field x , CInt
#field y , CInt
#field width , CInt
#field height , CInt
#stoptype

instance BoundingBox C'CvRect where
   type ELBB C'CvRect = Int
   bounds (C'CvRect x y w h) = Rectangle (f x) (f y) (f w) (f h)
    where f = fromIntegral

instance FromBounds C'CvRect where
   type ELFB C'CvRect = Int
   fromBounds (Rectangle x y w h) = C'CvRect (f x) (f y) (f w) (f h)
    where f = fromIntegral

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

#starttype CvConnectedComp
#field area, CDouble
#field value, <CvScalar>
#field rect, <CvRect>
#field contour, Ptr <CvSeq>
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
#field x , CFloat
#field y , CFloat
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


-- | /Spatial and central moments
#starttype CvMoments
-- | spatial moments
#field m00, CDouble
#field m10, CDouble
#field m01, CDouble
#field m20, CDouble
#field m11, CDouble
#field m02, CDouble
#field m30, CDouble
#field m21, CDouble
#field m12, CDouble
#field m03, CDouble
-- | central moments
#field mu20, CDouble
#field mu11, CDouble
#field mu02, CDouble
#field mu30, CDouble
#field mu21, CDouble
#field mu12, CDouble
#field mu03, CDouble
-- | @m00 != 0 ? 1/sqrt(m00) : 0@
#field inv_sqrt_m00, CDouble
#stoptype

-- | Hu invariants
#starttype CvHuMoments
#field hu1 , CDouble
#field hu2 , CDouble
#field hu3 , CDouble
#field hu4 , CDouble
#field hu5 , CDouble
#field hu6 , CDouble
#field hu7 , CDouble
#stoptype

#starttype CvTermCriteria
#field type, CInt
#field max_iter, CInt
#field epsilon, Double
#stoptype

data TermCriteria = EPS Double | ITER Int deriving (Show, Eq)

toCvTCrit (EPS d) = C'CvTermCriteria c'CV_TERMCRIT_EPS 0 d
toCvTCrit (ITER i) = C'CvTermCriteria c'CV_TERMCRIT_ITER (fromIntegral i) 0

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

#num CV_CLOCKWISE
#num CV_COUNTER_CLOCKWISE

#starttype CvConvexityDefect
#field start      , Ptr <CvPoint  
#field end        , Ptr <CvPoint>
#field depth_point, Ptr <CvPoint>
#field depth      , CFloat
#stoptype

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

