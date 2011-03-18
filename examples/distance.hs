{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Transforms
import CV.ColourUtils
import qualified CV.ImageMath as IM

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "distance.png"  $ montage (2,1) 2 
                                 [x
                                 ,stretchHistogram $ 
                                    distanceTransform CV_DIST_L2 M3 (IM.moreThan 0.6 x)
                                 ]

