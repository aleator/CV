{-#LANGUAGE ParallelListComp#-}
-- | This module provides wrappers for CvMat type. This is still preliminary as the type of the
--   matrix isn't coded in the haskell type.
module CV.Matrix 
    (
    Matrix, emptyMatrix ,fromList,toList,get,put,withMatPtr
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

-- #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )  \
--    (assert( (unsigned)(row) < (unsigned)(mat).rows &&   \
--             (unsigned)(col) < (unsigned)(mat).cols ),   \
--     (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))

-- | Haskell reflection of CvMat type
newtype Matrix = Matrix (ForeignPtr C'CvMat)

instance Show Matrix where
    show m = "fromList "++show (toList m)

matrixFinalizer ptr = with ptr c'cvReleaseMat

emptyMatrix :: (Int, Int) -> Matrix
emptyMatrix (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32FC1) 

withMatPtr :: Matrix -> (Ptr C'CvMat -> IO a) -> IO a
withMatPtr (Matrix m) op = withForeignPtr m op 

-- | Convert a list of floats into Matrix
fromList :: (Int,Int) -> [Float] -> Matrix
fromList (w,h) lst = unsafePerformIO $ do
                let Matrix e = emptyMatrix (w,h)
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d = c'CvMat'data'ptr mat'
                             s = c'CvMat'step mat'
                         sequence_ [putRaw d s row col v 
                                   | (row,col) <- [(r,c) | r <- [0..w-1], c <- [0..h-1]]
                                   | v <- lst ]
                return $ Matrix e

-- | Convert a matrix to flat list
toList :: Matrix -> [Float] 
toList (Matrix e) = unsafePerformIO $ do
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d = c'CvMat'data'ptr mat'
                             s = c'CvMat'step mat'
                             rows = fromIntegral $ c'CvMat'rows mat'
                             cols = fromIntegral $ c'CvMat'rows mat'
                         sequence [getRaw d s row col 
                                   | row <- [0..rows-1]
                                   , col <- [0..cols-1]
                                   ]

creatingMat fun = do
              iptr <- fun
              fptr <- newForeignPtr iptr (matrixFinalizer iptr)
              return . Matrix $ fptr

{-#INLINE get#-}
-- | Get an element of the matrix
get (Matrix m) row col = withForeignPtr m $ \mat -> do
         mat' <- peek mat
         let d = c'CvMat'data'ptr mat'
         let s = c'CvMat'step mat'
         getRaw d s row col
         --peek (castPtr (d `plusPtr` (col*(fromIntegral s)+row*sizeOf (undefined::Float))):: Ptr Float)

{-#INLINE getRaw#-}
getRaw d s row col = 
         peek (castPtr (d `plusPtr` (col*(fromIntegral s)+row*sizeOf (undefined::Float))):: Ptr Float)

{-#INLINE put#-}
-- | Write an element to a matrix
put (Matrix m) row col v = withForeignPtr m $ \mat -> do
         mat' <- peek mat
         let d = c'CvMat'data'ptr mat'
         let s = c'CvMat'step mat'
         putRaw d s row col v

{-#INLINE putRaw#-}
putRaw d s row col v = 
         poke (castPtr (d `plusPtr` (col*(fromIntegral s)+row*sizeOf (undefined::Float))):: Ptr Float)
              v

