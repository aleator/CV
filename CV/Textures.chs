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

calculateAsmAverage' :: Image c d -> CDouble
calculateAsmAverage' image = unsafePerformIO $ do
	       withGenImage image $ \img -> do
	         i <- {#call calculate_asm_average#} img
                 return i
{-



calculateAsmAverage :: Image c d -> IO ()
calculateAsmAverage image = unsafePerformIO $ do
	       withGenImage image $ \img -> do
	         vals <- {#call calculate_values#} img
                 print vals
-}

#c
typedef struct haralick_values haralick_values_t;
#endc

{#pointer *haralick_values_t as HaralickValues#}

-- {#fun calculate_values as ^
--  {`(Ptr (BareImage))'} -> `HaralickValuesPtr' id#} 

calculateValues img = withImage img {#call calculate_values#}

--data HaralickFeatures = HaralickFeatures { asm_average :: Double } deriving (Show)

calculateHaralickFeatures :: Image a b -> IO ()
calculateHaralickFeatures im = do
    values <- calculateValues im
    asmAverage' values >>= (\v->print $ "ASM average: "++(show v))
    asm0'       values >>= (\v->print $ "ASM 0':      "++(show v))
    asm45'      values >>= (\v->print $ "ASM 45':     "++(show v))
    asm90'      values >>= (\v->print $ "ASM 90':     "++(show v))
    asm135'     values >>= (\v->print $ "ASM 135':    "++(show v))
  where
    asmAverage' v = {#get haralick_values_t->asm_average#} v >>= return . realToFrac
    asm0'       v = {#get haralick_values_t->asm_0#}       v >>= return . realToFrac
    asm45'      v = {#get haralick_values_t->asm_45#}      v >>= return . realToFrac
    asm90'      v = {#get haralick_values_t->asm_90#}      v >>= return . realToFrac
    asm135'     v = {#get haralick_values_t->asm_135#}     v >>= return . realToFrac


