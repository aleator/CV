{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
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

lbp   = broilerPlate256 ({#call localBinaryPattern#})
lbpRI = broilerPlate' 32 ({#call localBinaryPattern#})

lbp3 = broilerPlate256 ({#call localBinaryPattern3#})
lbp5 = broilerPlate256 ({#call localBinaryPattern5#})
lbpHorizontal = broilerPlate256 
    ({#call localHorizontalBinaryPattern#})
lbpVertical = broilerPlate256 
    ({#call localVerticalBinaryPattern#})

-- LBP with weights and adjustable sampling points
weightedLBP offsetX offsetXY weights image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withGenImage weights $ \ws ->
                  withArray (replicate 256 0) $ \ptrn -> do
                    {#call weighted_localBinaryPattern#} img 
                        (fromIntegral offsetX) 
                        (fromIntegral offsetXY) ws ptrn 
                    p <- peekArray 256 ptrn
                    return p

broilerPlate256  = broilerPlate' 256
broilerPlate' l op image = unsafePerformIO $ do
             withGenImage image $ \img ->
              withArray (replicate l 0 :: [CInt]) $ \ptrn -> do
                (op img ptrn )
                p <- peekArray l ptrn
                let !maximum = fromIntegral $ sum p
                return $ map (\x -> fromIntegral x / maximum) p
