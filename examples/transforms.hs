module Main where
import CV.Image
import CV.ColourUtils
import CV.Transforms
import qualified CV.ImageMath as IM

remHigh x img = IM.mul img  (unsafeImageTo32F $ IM.moreThan x img)

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "transforms.png" $ montage (3,2) 5 $
        [rotate (pi/3.1) x
        ,montage (2,2) 1 $ [scaleSingleRatio NearestNeighbour 0.48 x
                           ,scaleSingleRatio Linear 0.48 x
                           ,scaleSingleRatio Area 0.48 x
                           ,scaleSingleRatio Cubic 0.48 x]
        ,radialDistort x 0.7
        ,stretchHistogram $ IM.log $ dct $ evenize x
        ,stretchHistogram $ idct $ remHigh 0.2 $ dct $ evenize x
        ,perspectiveTransform x [0.8,0,0.2, 0.2,1,0.1, 0, 0, 1]
        ]

