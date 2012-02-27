module Main where
import CV.Fitting
import CV.Image
import CV.Drawing
import CV.Matrix
import CV.ImageOp

main = do
        let res :: Image GrayScale D32
            res = empty (400,400)
            testPts = [(200+100*sin x-80*cos x,200+60*cos x) | x <- [0,0.1..2*pi]]
            mat = fromList (1,length testPts) testPts
            ell = fitEllipse mat
            bb  = minAreaRect mat
            br  = boundingRect mat
            pts = res <## [circleOp 1 (round x, round y) 3 Filled | (x,y) <- testPts]
                      <# drawBox2Dop 1 bb
        saveImage "bb_result.png" pts
        print ell
        print bb
        print br
