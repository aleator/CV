module Main where
import CV.Fitting
import CV.Image
import CV.ConnectedComponents
import CV.Drawing
import CV.Matrix
import CV.ImageOp
import CV.Bindings.Types
import System.Environment

main' = do
        let res :: Image RGB D32
            res = empty (400,400)
            testPts = [(200+100*sin x-80*cos x,200+60*cos x) | x <- [0,0.1..pi]] ++
                      [(150+100*sin (-x)+20*cos x,100+60*cos (-x)) | x <- [0,0.1..2*pi]]
            mat = fromList (1,length testPts) testPts
            ell = fitEllipse mat
            bb  = minAreaRect mat
            br  = boundingRect mat
            ch  = map (both round) . toList $ convexHull mat 
            segments = zip (ch) (tail . cycle $ ch)
            pts = res <## [circleOp (0.5,0.5,0.5) (round x, round y) 3 Filled | (x,y) <- testPts]
                      <#  drawLinesOp (1,0,0) 1 segments
                      <# drawBox2Dop (0,1,0) bb
        saveImage "bb_result.png" pts
        print ell
        print bb
        print br

main = do
    Just x <- getArgs >>= loadImage . head
    let 
        cs = head . mapContours contourPoints . getContours . unsafeImageTo8Bit $ x 
        mat :: Matrix (Float,Float)
        mat = fromList (1,length cs) (map (both realToFrac) cs)
        matI :: Matrix (Int,Int)
        matI = fromList (1,length cs) (map (both round) cs)
        ell = fitEllipse mat
        bb  = minAreaRect mat
        br  = boundingRect mat
        ch  = map (both round) . toList $ convexHull mat 
        cdefs = convexityDefects matI
        segments = zip (ch) (tail . cycle $ ch)
        pts = grayToRGB x 
                  <## [circleOp (0.5,0.5,0.5) (round x, round y) 3 Filled | (x,y) <- toList mat]
                  <## [circleOp (1,0,0.8) (fromIntegral x, fromIntegral y) 13 Filled 
                      | (_,_,C'CvPoint x y,_) <- cdefs]
                  <#  drawLinesOp (1,0,0) 3 segments
                  <#  drawBox2Dop (0,1,0) bb
    mapM_ print cdefs
    saveImage "bb_result.png" pts


both f (a,b)  = (f a, f b)
