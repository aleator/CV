{-#LANGUAGE ParallelListComp,ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Drawing
import CV.ImageOp
import CV.Transforms
import qualified CV.ImageMath as IM

main = do
    let s :: Image GrayScale D32
        s = empty (300,300) <# circleOp 1 (150,150) 40 Filled 
        dtf = distanceTransform CV_DIST_L2 M3  (unsafeImageTo8Bit s)
        testCircle = (170,160,10)
        (mx,my,r) = IM.maximalCoveringCircle dtf testCircle

    saveImage "cover.png" $ montage (3,1) 2 
            [s
            ,(unsafeImageTo32F s) <# circleOp 0 (170,160) 10 Filled
            ,empty (300,300)      <# circleOp 1 (mx,my) (round r) (Stroked 2) ]

