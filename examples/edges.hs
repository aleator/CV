module Main where
import CV.Image
import CV.Edges

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "edges.png" $ montage (3,2) 5 $
        [x
        ,sobel (1,0) s5 x
        ,sobel (0,1) s5 x
        ,laplace l5 x
        ,unsafeImageTo32F $ susan (5,5) 0.5 x
        ,unsafeImageTo32F $ canny 20 40 5 (unsafeImageTo8Bit x)
        ]
