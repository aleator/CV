{-#LANGUAGE ParallelListComp, TypeFamilies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables#-}
-- | This module provides wrappers for CvMat type. This is still preliminary as the type of the
--   matrix isn't coded in the haskell type.
module CV.Matrix
    (
    Exists(..),
    Matrix, emptyMatrix ,fromList,toList,toRows,toCols,get,put,withMatPtr
    , transpose, mxm, rodrigues2
    )where

{-#OPTIONS_GHC -fwarn-unused-imports#-}

import System.Posix.Files
import System.Mem

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Storable.Tuple
import Control.Parallel.Strategies
import Control.DeepSeq

-- import C2HSTools

import Data.Maybe(catMaybes)
import Data.List(genericLength)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Control.Monad

import CV.Bindings.Matrix
import CV.Bindings.Types
import CV.Image hiding (create)

-- #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )  \
--    (assert( (unsigned)(row) < (unsigned)(mat).rows &&   \
--             (unsigned)(col) < (unsigned)(mat).cols ),   \
--     (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))

-- | Haskell reflection of CvMat type
newtype Matrix a = Matrix (ForeignPtr C'CvMat)

instance (Show t, Storable t, (Size (Matrix t))~(Int,Int)) => Show (Matrix t) where
    show m = "fromList "++show (getSize m)++" "++show (toList m)

matrixFinalizer ptr = with ptr c'cvReleaseMat

class Exists a where
    type Args a :: *
    create :: Args a -> a

instance Exists (Matrix Float) where
    type Args (Matrix Float) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32FC1)

instance Exists (Matrix Int) where
    type Args (Matrix Int) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32SC1)

instance Exists (Matrix (Float,Float)) where
    type Args (Matrix (Float,Float)) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32FC2)

instance Exists (Matrix (Float,Float,Float)) where
    type Args (Matrix (Float,Float,Float)) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32FC3)

instance Exists (Matrix (Int,Int,Int,Int)) where
    type Args (Matrix (Int,Int,Int,Int)) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32SC4)

instance Exists (Matrix Double) where
    type Args (Matrix Double) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_64FC1)

instance Sized (Matrix a) where
    type Size (Matrix a) = (Int,Int)
    getSize (Matrix e) = unsafePerformIO $ withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         return (fromIntegral $ c'CvMat'rows mat',
                                fromIntegral $ c'CvMat'cols mat')

-- | Create an empty matrix of given dimensions
emptyMatrix :: Exists (Matrix a) => Args (Matrix a) -> Matrix a
emptyMatrix a = create a

identity :: (Num a, Sized (Matrix a), Args (Matrix a) ~ (Int,Int),  Size (Matrix a) ~ (Int,Int),  Storable a, Exists (Matrix a)) =>
             (Matrix a) -> Matrix a
identity a = unsafePerformIO $ do
             let res = create (getSize a)
                 (rows,cols) = getSize a
             sequence_ [put res row col 1
                       | row <- [0..rows-1]
                       | col <- [0..cols-1]]
             return res

-- | Transpose a matrix. Does not do complex conjugation for complex matrices
transpose :: (Exists (Matrix a), Args (Matrix a) ~ Size (Matrix a)) => Matrix a -> Matrix a
transpose m@(Matrix f_m) = unsafePerformIO $ do
                 let res@(Matrix f_c) = create (getSize m)
                 withForeignPtr f_m $ \c_m ->
                  withForeignPtr f_c $ c'cvTranspose c_m
                 return res

-- | Convert a rotation vector to a rotation matrix (1x3 -> 3x3)
rodrigues2 :: (Exists (Matrix a), Args (Matrix a) ~ Size (Matrix a)) => Matrix a -> Matrix a
rodrigues2 m@(Matrix f_m) = unsafePerformIO $ do
                 let res@(Matrix f_c) = create (3,3)
                 withForeignPtr f_m $ \c_m ->
                  withForeignPtr f_c $ \c_c -> c'cvRodrigues2 c_m c_c nullPtr
                 return res


-- | Ordinary matrix multiplication
mxm :: (Exists (Matrix a), Args (Matrix a) ~ Size (Matrix a)) => Matrix a -> Matrix a -> Matrix a
mxm m1@(Matrix a_m) m2@(Matrix b_m) = unsafePerformIO $ do
                 let (w1,h1) = getSize m1
                     (w2,h2) = getSize m2
                     res@(Matrix f_c) = create (w1,h2)
                 when (h1 /= w2) . error  $
                    "Matrix dimensions do not match for multiplication: "
                    ++show (w1,h1)
                    ++" vs. "
                    ++ show (w2,h2)
                 withForeignPtr a_m $ \c_a ->
                  withForeignPtr b_m $ \c_b ->
                  withForeignPtr f_c $ \c_res -> c'cvGEMM c_a c_b 1 nullPtr 1 c_res 0
                 return res

withMatPtr :: Matrix x -> (Ptr C'CvMat -> IO a) -> IO a
withMatPtr (Matrix m) op = withForeignPtr m op

-- | Convert a list of floats into Matrix
fromList :: forall t . (Storable t, Exists (Matrix t), Args (Matrix t) ~ (Int,Int))
                        => (Int,Int) -> [t] -> Matrix t
fromList (w,h) lst = unsafePerformIO $ do
                let m@(Matrix e) = emptyMatrix (w,h)
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d :: Ptr t
                             d = castPtr $ c'CvMat'data'ptr mat'
                             s = c'CvMat'step mat'
                             size = sizeOf (undefined :: t)
                         sequence_ [putRaw d s size row col v
                                   | (row,col) <- [(r,c) | c <- [0..h-1], r <- [0..w-1]]
                                   | v <- lst ]
                return $ m


-- | Convert a matrix to flat list (row major order)
toList :: (Storable a) => Matrix a -> [a]
toList =  concat . toRows

-- | Convert matrix to rows represented as nested lists
toRows :: forall t . (Storable t) => Matrix t -> [[t]]
toRows (Matrix e) = unsafePerformIO $ do
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d = castPtr (c'CvMat'data'ptr mat') :: Ptr t
                             s = c'CvMat'step mat'
                             rows = fromIntegral $ c'CvMat'rows mat'
                             cols = fromIntegral $ c'CvMat'cols mat'
                         sequence [sequence [getRaw d (fromIntegral s) row col | row <- [0..rows-1]]
                                   | col <- [0..cols-1]
                                   ]

-- | Convert matrix to cols represented as nested lists
toCols :: forall t . (Storable t) => Matrix t -> [[t]]
toCols (Matrix e) = unsafePerformIO $ do
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d = castPtr (c'CvMat'data'ptr mat') :: Ptr t
                             s = c'CvMat'step mat'
                             rows = fromIntegral $ c'CvMat'rows mat'
                             cols = fromIntegral $ c'CvMat'cols mat'
                         sequence [sequence [getRaw d (fromIntegral s) row col | col <- [0..cols-1]]
                                   | row <- [0..rows-1]
                                   ]

creatingMat fun = do
              iptr <- fun
              fptr <- newForeignPtr iptr (matrixFinalizer iptr)
              return . Matrix $ fptr

{-#INLINE get#-}
-- | Get an element of the matrix
get :: forall t . (Storable t) => (Matrix t) -> Int -> Int -> IO t
get (Matrix m) row col = withForeignPtr m $ \mat -> do
         mat' <- peek mat
         let d = c'CvMat'data'ptr mat'
         let s = c'CvMat'step mat'
         getRaw (castPtr d:: Ptr t) (fromIntegral s) (fromIntegral row) (fromIntegral col)

{-#INLINE getRaw#-}
getRaw :: forall t . (Storable t) => Ptr t -> Int -> Int -> Int -> IO t
getRaw d s col row =
         peek (castPtr (d `plusPtr` (col*s+row*sizeOf (undefined::t))):: Ptr t)

{-#INLINE put#-}
-- | Write an element to a matrix
put :: forall t . (Storable t) => (Matrix t) -> Int -> Int -> t -> IO ()
put (Matrix m) row col v = withForeignPtr m $ \mat -> do
         mat' <- peek mat
         let
            d :: Ptr t
            d = castPtr $ c'CvMat'data'ptr mat'
            s = c'CvMat'step mat'
            size = sizeOf (undefined :: t)
         putRaw d s size row col v

{-#INLINE putRaw#-}
putRaw :: forall t. (Storable t) => Ptr t -> CInt -> Int -> Int -> Int -> t -> IO ()
putRaw d step eltSize col row v =
         poke (castPtr (d `plusPtr` (col*(fromIntegral step)+row*eltSize))) v

