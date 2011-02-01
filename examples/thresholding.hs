module Main where
import CV.Image
import CV.Thresholding

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "thresholding.png" $ montage (3,2) 5 $
        [x
        ,unsafeImageTo32F $ nibbly 1.2 0.01 x
        ,unsafeImageTo32F $ otsu 64 x
        ,unsafeImageTo32F $ kittler 0.1 x
        ,unsafeImageTo32F $ bernsen (9,9) 0.2 x
        ]

