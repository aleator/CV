-- |This module contains functions for simple histogram manipulation. Use this
-- to scale the image for viewing or to perform simple light-level normalization
-- accross multiple images.
{-#LANGUAGE ForeignFunctionInterface,ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.ColourUtils (
                        balance
                      , logarithmicCompression
                      , stretchHistogram
                      , equalizeHistogram  
                )
where
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

-- |Adjust the image histogram to have fixed mean and standard deviation. This can
--  be used for simple light level normalization.
balance :: (D32, D32) -> Image GrayScale D32 -> Image GrayScale D32
balance (m,md) i = m |+ (scale |* (i |- im) ) 
    where
        imd :: D32 = realToFrac $ IM.stdDeviation i
        im  :: D32 = IM.average i
        scale :: D32 = realToFrac $ md/imd

-- |Perform logarithmic compression on the image. This will enhance dark features
--  and suppress bright features. Use this to visualize images with high dynamic range. 
--  (FFT results, for example)
logarithmicCompression :: Image GrayScale D32 -> Image GrayScale D32
logarithmicCompression image = stretchHistogram $ IM.log $  1 `IM.addS`  image  

-- |Histogram stretch scales the image to fit the range [0,1]
stretchHistogram :: Image GrayScale D32 -> Image GrayScale D32 
stretchHistogram image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax image

-- | Equalize contrast of the image. This is good for visualizing 
--   images with backgrounds and foregrounds that are both bright or both dark.
equalizeHistogram :: Image GrayScale D8 -> Image GrayScale D8
equalizeHistogram image = unsafePerformIO $ do
                       withClone image $ \x ->
                        withGenImage x $ \i ->
                            {#call cvEqualizeHist#} i i

getStretchScaling :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
getStretchScaling reference image = stretched
            where
             stretched = (1/realToFrac length) `IM.mulS` normed
             normed = image `IM.subS` (realToFrac min)
             length = max-min
             (min,max) = IM.findMinMax reference



