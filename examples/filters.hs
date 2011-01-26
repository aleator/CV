module Main where
import CV.Image
import CV.Filters

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "filters.png" $ montage (3,2) 5 $
        [x
        ,gaussian (9,9) x
        ,blur (9,9) x
        ,haar (integralImage x) (0,0,10,5)
        ,unsafeImageTo32F $ bilateral 3 9 (unsafeImageTo8Bit x)
        ,unsafeImageTo32F $ median (9,9) (unsafeImageTo8Bit x)
        ]

