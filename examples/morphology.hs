module Main where
import CV.Image
import CV.Morphology

main = do
    Just x <- loadImage "smallLena.jpg"
    saveImage "morphology.png" $ montage (3,3) 5 $
        [x
        ,erode basicSE 2 x
        ,dilate basicSE 2 x
        ,blackTopHat 5 x
        ,whiteTopHat 5 x
        ,open basicSE x
        ,close basicSE x
        ,close (structuringElement (2,8) (1,4) crossShape) x
        ,open (structuringElement (2,8) (1,4)  crossShape) x
        ]

