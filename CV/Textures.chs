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

#c
typedef struct haralick_values haralick_values_t;
#endc

{#pointer *haralick_values_t as HaralickValues#}

calculateValues :: CV.Image.Image c d -> Ptr ()
calculateValues img = unsafePerformIO $ withImage img {#call calculate_values#}

data HaralickFeatures = HaralickFeatures {
                          asms :: [Double],        -- Angular second moments at four angles
                          contrasts :: [Double],   -- Contrasts at four angles
                          correlations :: [Double] -- Correlations at four angles
                        } deriving(Show)

calculateHaralickFeatures :: Image a b -> HaralickFeatures
calculateHaralickFeatures im = HaralickFeatures asms contrasts correlations
  where
    v = calculateValues im
    asm0'        = unsafePerformIO $ {#get haralick_values_t->asm_0#}   v >>= return . realToFrac
    asm45'       = unsafePerformIO $ {#get haralick_values_t->asm_45#}  v >>= return . realToFrac
    asm90'       = unsafePerformIO $ {#get haralick_values_t->asm_90#}  v >>= return . realToFrac
    asm135'      = unsafePerformIO $ {#get haralick_values_t->asm_135#} v >>= return . realToFrac
    asms         = [asm0', asm45', asm90', asm135']
    contrast0'   = unsafePerformIO $ {#get haralick_values_t->contrast_0#}   v >>= return . realToFrac
    contrast45'  = unsafePerformIO $ {#get haralick_values_t->contrast_45#}  v >>= return . realToFrac
    contrast90'  = unsafePerformIO $ {#get haralick_values_t->contrast_90#}  v >>= return . realToFrac
    contrast135' = unsafePerformIO $ {#get haralick_values_t->contrast_135#} v >>= return . realToFrac
    contrasts    = [contrast0', contrast45', contrast90', contrast135'] 
    correlation0'   = unsafePerformIO $ {#get haralick_values_t->correlation_0#}   v >>= return . realToFrac
    correlation45'  = unsafePerformIO $ {#get haralick_values_t->correlation_45#}  v >>= return . realToFrac
    correlation90'  = unsafePerformIO $ {#get haralick_values_t->correlation_90#}  v >>= return . realToFrac
    correlation135' = unsafePerformIO $ {#get haralick_values_t->correlation_135#} v >>= return . realToFrac
    correlations    = [correlation0', correlation45', correlation90', correlation135'] 
