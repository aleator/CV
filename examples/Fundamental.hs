{-#LANGUAGE CPP#-}
import qualified CV.Matrix as M
import CV.Calibration


main = do
    let mat   = M.fromList (2,10) $ concat [[x,y] | x <- [1,2], y <- [1..5]]
        mat2  = M.fromList (1,10) $ [(x,y) | x <- [1,2], y <- [1..5]]
        a     = findFundamentalMat mat mat c'CV_FM_RANSAC 1 0.6
        (c,d) = stereoRectifyUncalibrated mat2 mat2 a (30,30) 0.5
    print a
    print c
    print d
