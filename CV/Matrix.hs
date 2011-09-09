{-#LANGUAGE ParallelListComp, TypeFamilies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables#-}
-- | This module provides wrappers for CvMat type. This is still preliminary as the type of the
--   matrix isn't coded in the haskell type.
module CV.Matrix 
    (
    Matrix, emptyMatrix ,fromList,toList,toLists,get,put,withMatPtr
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
import CV.Image hiding (create)

-- #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )  \
--    (assert( (unsigned)(row) < (unsigned)(mat).rows &&   \
--             (unsigned)(col) < (unsigned)(mat).cols ),   \
--     (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))

-- | Haskell reflection of CvMat type
newtype Matrix a = Matrix (ForeignPtr C'CvMat)

instance (Show t, Storable t) => Show (Matrix t) where
    show m = "fromList "++show (toLists m)

matrixFinalizer ptr = with ptr c'cvReleaseMat

class Creatable a where
    type Args a :: *
    create :: Args a -> a

instance Creatable (Matrix Float) where
    type Args (Matrix Float) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32FC1) 

instance Creatable (Matrix Int) where
    type Args (Matrix Int) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_32SC1) 

instance Creatable (Matrix Double) where
    type Args (Matrix Double) = (Int,Int)
    create (r,c) = unsafePerformIO $ creatingMat (c'cvCreateMat r c c'CV_64FC1) 

emptyMatrix :: Creatable (Matrix a) => Args (Matrix a) -> Matrix a
emptyMatrix a = create a

withMatPtr :: Matrix x -> (Ptr C'CvMat -> IO a) -> IO a
withMatPtr (Matrix m) op = withForeignPtr m op 

-- | Convert a list of floats into Matrix
fromList :: forall t . (Storable t, Creatable (Matrix t), Args (Matrix t) ~ (Int,Int)) 
                        => (Int,Int) -> [t] -> Matrix t
fromList (w,h) lst = unsafePerformIO $ do
                let m@(Matrix e) = emptyMatrix (w,h)
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d :: Ptr t
                             d = castPtr $ c'CvMat'data'ptr mat'
                             s = c'CvMat'step mat'
                         sequence_ [putRaw d s row col v 
                                   | (row,col) <- [(r,c) | c <- [0..h-1], r <- [0..w-1]]
                                   | v <- lst ]
                return $ m

instance IntSized (Matrix a) where
    getSize (Matrix e) = unsafePerformIO $ withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         return (fromIntegral $ c'CvMat'rows mat', 
                                fromIntegral $ c'CvMat'cols mat')

-- | Convert a matrix to flat list
toList :: Matrix Float -> [Float] 
toList =  concat . toLists

-- | Convert matrix to nested lists
toLists :: forall t . (Storable t) => Matrix t -> [[t]] 
toLists (Matrix e) = unsafePerformIO $ do
                withForeignPtr e $ \mat -> do
                         mat' <- peek mat
                         let d = castPtr (c'CvMat'data'ptr mat') :: Ptr t
                             s = c'CvMat'step mat'
                             rows = fromIntegral $ c'CvMat'rows mat'
                             cols = fromIntegral $ c'CvMat'cols mat'
                         sequence [sequence [getRaw d (fromIntegral s) row col | row <- [0..rows-1]]
                                   | col <- [0..cols-1]
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
         putRaw d s row col v

{-#INLINE putRaw#-}
putRaw :: forall t. (Storable t) => Ptr t -> CInt -> Int -> Int -> t -> IO ()
putRaw d s col row v = 
         poke (castPtr (d `plusPtr` (col*(fromIntegral s)+row*sizeOf (undefined::t))):: Ptr t)
              v

