{-#LANGUAGE ParallelListComp,ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.ImageOp
import CV.Drawing
import CV.ColourUtils
import CV.HoughTransform
import CV.Matrix

main = do
    Just x <- loadImage "houghtest2.png"
    let hough = houghLinesStandard (unsafeImageTo8Bit x) 100 1 (pi/180) 100
        hough2 = houghLinesProbabilistic (unsafeImageTo8Bit x) 100 1 (pi/180) 100 10 40
        hough3 = houghLinesMultiscale (unsafeImageTo8Bit x) 100 5 (pi/90) 100 4 4

        (w,h) = getSize x
        test = x <## [lineOp 1 2 (0,round y) (w,round $ y + (fromIntegral w)*negate (tan (pi/2-k))) | (y,k) <- toList hough]
        test2 = x <## [lineOp 1 2 ( x, y) ( u, v) | (x,y,u,v) <- toList hough2]
        test3 = x <## [lineOp 1 2 (0,round y) (w,round $ y + (fromIntegral w)*negate (tan (pi/2-k))) | (y,k) <- toList hough3]
    print hough
    print hough2
    print hough3
    saveImage "Hough_result.png" test
    saveImage "Hough_result2.png" test
    saveImage "Hough_result3.png" test3
    
