module CV.PatternRemover where
import CV.Image
import CV.Transforms
import CV.ColourUtils
import qualified CV.ImageMath as IM
import CV.ImageMathOp

import CV.Filters
import CV.Thresholding
import CV.Drawing
import CV.Morphology

-- Remove pattern creates a filter that will remove regular patterns
-- from images. The assumption is that the image is, for practical
-- purposes, homogenous and contains slight texture which is undesired.
-- For sellu series: removePattern 7 13 50 0.6  $ getRegion (0,0) (304,304) x
-- Parameters: 
-- Smoothing size, spike size, minimum filtered frequency, amount of
-- removal, image
designFilter s2 min w img = filter
    where
     ci = dct img
     filter = circle (0,0) min 0 Filled filter'
     filter' = dilate basicSE 2 $ nibbly w 0.00001 $
                gaussian (s2,s2) $ IM.log ci -- #- ci

                
removePattern s1 s2 min w img = idct (ci #* IM.invert filter)
    where
     ci = dct img
     filter = circle (0,0) min 0 Filled filter'
     filter' = gaussian (s1,s1) $ dilate basicSE 2 $ nibbly w 0.00001 
                ci #- gaussian (s2,s2) ci

