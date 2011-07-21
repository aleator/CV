module Main where
import CV.Image
import CV.Edges
import qualified CV.ImageMath as IM
import CV.ColourUtils

main = do
    Just x <- loadImage "smallLena.jpg"
    let 
        dx = sobel (1,0) s5 x
        dy = sobel (0,1) s5 x
    saveImage "edges.png" $ montage (1,7) 5 $
        [x
        ,sobel (1,0) s5 x
        ,sobel (0,1) s5 x
        ,laplace l5 x
        ,unsafeImageTo32F $ susan (5,5) 0.5 x
        ,unsafeImageTo32F $ canny 20 40 5 (unsafeImageTo8Bit x)
        ,stretchHistogram $ IM.atan2 dy dx
        ]
