module Main where
import CV.Image
import CV.ColourUtils
import CV.Edges
import Data.Maybe

main = do
    x <- loadImage "smallLena.jpg" >>= return . sobel (2,0) s5 . fromJust
    saveImage "colourUtils.png" $ montage (5,1) 5 $
        [x
        ,balance (0.5,0.2)  x
        ,logarithmicCompression x
        ,stretchHistogram x
        ,unsafeImageTo32F $ equalizeHistogram (unsafeImageTo8Bit x)
        ]

