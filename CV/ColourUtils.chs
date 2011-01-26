{-#LANGUAGE ForeignFunctionInterface,ScopedTypeVariables#-}
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

-- TODO: Rename this entire module to something else. Everything here  is grayscale :/

-- Balance image grayscales so that it has m mean and md standard deviation
balance (m,md) i = m |+ (scale |* (i |- im) ) 
    where
        imd :: D32 = realToFrac $ IM.stdDeviation i
        im  :: D32 = IM.average i
        scale :: D32 = realToFrac $ md/imd


logarithmicCompression image = stretchHistogram $ 
                                IM.log $  1 `IM.addS`  image  


getStretchScaling reference image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax reference


stretchHistogram :: Image GrayScale D32 -> Image GrayScale D32 
stretchHistogram image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax image

equalizeHistogram :: Image GrayScale D8 -> Image GrayScale D8
equalizeHistogram image = unsafePerformIO $ do
                       withClone image $ \x ->
                        withGenImage x $ \i ->
                            {#call cvEqualizeHist#} i i

