{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
#include "haralick.h"

module CV.Textures where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array

import CV.Image 
import CV.ImageOp

import C2HSTools
{#import CV.Image#}


-- | Various simple Local Binary Pattern operators

lbp = broilerPlate ({#call localBinaryPattern#})

lbp3 = broilerPlate ({#call localBinaryPattern3#})
lbp5 = broilerPlate ({#call localBinaryPattern5#})
lbpHorizontal = broilerPlate 
    ({#call localHorizontalBinaryPattern#})
lbpVertical = broilerPlate 
    ({#call localVerticalBinaryPattern#})

-- LBP with weights and adjustable sampling points

-- weightedLBP :: :: (Integral a) =>
--   a -> a ->  Image c1 d1 -> Image c d -> [CDouble]
weightedLBP offsetX offsetXY weights image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withGenImage weights $ \ws ->
                  withArray (replicate 256 0) $ \ptrn -> do
                    {#call weighted_localBinaryPattern#} img (fromIntegral offsetX) (fromIntegral offsetXY) ws ptrn 
                    p <- peekArray 256 ptrn
                    return p

emptyPattern :: [CInt]
emptyPattern = replicate 256 0
broilerPlate op image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withArray emptyPattern $ \ptrn -> do
                (op img ptrn )
                p <- peekArray 256 ptrn
                let !maximum = fromIntegral $ sum p
                return $ map (\x -> fromIntegral x / maximum) p


