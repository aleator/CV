{-#LANGUAGE ForeignFunctionInterface, ViewPatterns#-}
#include "cvWrapLEO.h"
#include "haralick.h"

-- | This module implements some haralick features. The design philosophy is that haralick features
--   are always calculated for [0,1] range, grayscale eight bit images. To get the images in this
--   condition one can use the functions from CV.Image module.
module CV.Haralick where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified Data.Array.CArray as CA

import CV.Image 
import CV.ImageOp

import C2HSTools
{#import CV.Image#}

-- * Haralick features

-- | Co-Occurence matrix is currently a CArray. It might be nice to use REPA array instead
type CoOccurenceMatrix = CA.CArray (Int,Int) Double


-- | Calculate CoOccurenceMatrix with a given offset.
coOccurenceMatrix :: (Int,Int) ->  Image GrayScale D8 -> CoOccurenceMatrix 
coOccurenceMatrix (fromIntegral -> dx, fromIntegral -> dy) image = unsafePerformIO $
    withGenImage image $ \cimg -> CA.createCArray ((0,0),(255,255)) $ calcCoOc cimg dx dy


foreign import ccall unsafe "cbits/haralick.h calculate_co_occurence_matrix" 
   calcCoOc :: Ptr BareImage -> Int -> Int -> Ptr Double -> IO ()


-- | Calculate a contrast measure of a co-occurence matrix
{#fun calculate_contrast as contrast 
    {withArrPtr* `CoOccurenceMatrix', `Int'} -> `Double' #}

-- | Calculate a second angular moment measure of a co-occurence matrix
{#fun calculate_asm      as angularSecondMoment 
    {withArrPtr* `CoOccurenceMatrix', `Int'} -> `Double' #}

-- TODO: withArrPtr should somehow be able to supply the size as well.

withArrPtr arr o = let (i,p) = CA.toForeignPtr arr 
                   in do 
                       r <- o (castPtr $ unsafeForeignPtrToPtr p) 
                       touchForeignPtr p
                       return r

