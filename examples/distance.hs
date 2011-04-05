{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Transforms
import CV.ColourUtils
import qualified CV.ImageMath as IM
import CV.Drawing
import CV.ImageOp

import Data.Maybe (fromJust)

main = do
    x <- loadImage "smallLena.jpg" >>= return . IM.moreThan 0.6 . fromJust
    let dtf = distanceTransform L2 M3 x
        (_,((mx,my),v)) = IM.findMinMaxLoc dtf
    saveImage "distance.png"  $ montage (3,1) 2 
                                 [unsafeImageTo32F x
                                 ,(stretchHistogram $ dtf)
                                 ,(stretchHistogram $ dtf) <# circleOp 1 (mx,my) (round v) Filled
                                 ]

