module Main where

import CV.Image
import CV.Tracking
import CV.Drawing
import CV.Bindings.Types

main = do
   Just x <- loadImage "smallLena.jpg"
   let initPts = [(1,x) | x <- [1,20..200]] ++
                 [(200,x) | x <- [1,20..200]] ++
                 [(x,200) | x <- [1,20..200]]
   print initPts
   s <- snake (unsafeImageTo8Bit x) initPts 3 3 3 (20,20) (C'CvTermCriteria c'CV_TERMCRIT_ITER 100 0) False
   mapM_ print s
