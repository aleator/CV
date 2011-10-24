{-#LANGUAGE ForeignFunctionInterface, ParallelListComp#-}
#include "cvWrapLEO.h"
module CV.Textures where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array

import CV.Image 
import CV.ImageOp

import Data.List
import C2HSTools
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
{#import CV.Image#}


-- * Rotation invariance

-- |Normalize Word8 according to lbp-logic.
normalize :: Word8 -> Word8
normalize x = minimum [rotateL x i | i<-[0..8] ]

-- |Make an lbp table element rotation invariant
rotationInvariantE :: Word8 -> Word8 
rotationInvariantE a =  fromIntegral
                      . fromMaybe (error "Unthinkable happened(RI)") 
                      . V.findIndex (==normalize a) $ keywords
keywords = V.fromList . nub . sort . map normalize $ allWords
allWords = [minBound .. maxBound]

-- Convert an LBP histogram into rotation invariant form
rotationInvariant :: [Double] -> Vector Double
rotationInvariant es = V.accum (+) (V.replicate 36 0) 
                         [(fromIntegral . rotationInvariantE $ i, e) 
                         | i <- [minBound .. maxBound]
                         | e <- es]
            

-- | Various simple Local Binary Pattern operators
lbp :: Image GrayScale D32 -> [Double] 
lbp   = broilerPlate256 ({#call localBinaryPattern#})

lbp3 :: Image GrayScale D32 -> [Double] 
lbp3 = broilerPlate256 ({#call localBinaryPattern3#})

lbp5 :: Image GrayScale D32 -> [Double] 
lbp5 = broilerPlate256 ({#call localBinaryPattern5#})

lbpHorizontal :: Image GrayScale D32 -> [Double] 
lbpHorizontal = broilerPlate256 
    ({#call localHorizontalBinaryPattern#})

lbpVertical :: Image GrayScale D32 -> [Double] 
lbpVertical = broilerPlate256 
    ({#call localVerticalBinaryPattern#})

weightedLBP :: (Integral a, Integral a1) =>
     a
     -> a1
     -> CV.Image.Image GrayScale D32
     -> CV.Image.Image GrayScale D32
     -> [Double]

weightedLBP offsetX offsetXY weights image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withGenImage weights $ \ws ->
                  withArray (replicate 256 0) $ \ptrn -> do
                    {#call weighted_localBinaryPattern#} img 
                        (fromIntegral offsetX) 
                        (fromIntegral offsetXY) ws ptrn 
                    p <- peekArray 256 ptrn
                    return $ map realToFrac p

broilerPlate256  = broilerPlate' 256
broilerPlate' l op image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withArray (replicate l 0 :: [CInt]) $ \ptrn -> do
                (op img ptrn )
                p <- peekArray l ptrn
                let !maximum = fromIntegral $ sum p
                return $ map (\x -> fromIntegral x / maximum) p
