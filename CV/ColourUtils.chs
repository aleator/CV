{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.ColourUtils where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.Image 
{#import CV.Image#}
import CV.ImageOp
import qualified CV.ImageMath as IM
import CV.ImageMathOp

import C2HS

-- Balance image grayscales so that it has m mean and md standard deviation
balance (m,md) i = m |+ (scale |* (i |- im) ) 
    where
        imd = realToFrac $ IM.stdDeviation i
        im  = IM.average i
        scale = realToFrac $ md/imd


logarithmicCompression image = stretchHistogram $ 
                                IM.log $  1 `IM.addS`  image  


getStretchScaling reference image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax reference


stretchHistogram image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax image

equalizeHistogram image = unsafePerformIO $ do
                       x <- imageTo8Bit image
                       withGenImage x $ \i ->
                            {#call cvEqualizeHist#} i i
                       imageTo32F x

