{-#LANGUAGE ScopedTypeVariables #-}
module Main where
import CV.Image
import CV.MultiresolutionSpline 
import CV.ImageOp
import CV.Drawing
import CV.Filters
import qualified CV.Transforms as T

main = do
    Just x <- loadImage "smallLena.jpg"
    Just y <- loadImage "elaine.jpg"
    m :: Image GrayScale D32 <- create (192,192)
    let gr = getRegion (0,0) (192,192)
        mask = (unsafeImageTo8Bit $ gaussian (3,3) $ m <# rectOp 1 (-1) (0,0) (90,192))
    saveImage "spline.png" $ montage (4,1) 5 $
        [gr x
        ,gr y
        ,unsafeImageTo32F mask
        ,burtAdelsonMerge 4 mask  (gr x)  (T.flip T.Horizontal $ gr y)
        ]

