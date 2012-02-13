module Main where
import CV.Image
import CV.Features
import CV.Drawing
import CV.ImageOp
import CV.Bindings.Types
import CV.Transforms

main = do
   Just x <- loadImage "smallLena.jpg"
   let y = rotate (pi/2) x
   lst <- getSURF defaultParams (unsafeImageTo8Bit x)
   lsty <- getSURF defaultParams (unsafeImageTo8Bit y)
   let result lst x = x <## [circleOp 1 (round x,round y) 6 (Stroked 1)
                      | (pt,_) <- lst
                      , let C'CvPoint2D32f x y = c'CvSURFPoint'pt pt ]
   saveImage "surf_result.png" $ montage (2,1) 2 [result lst x ,result lsty y]
   mapM_ print (take 5 lst)

-- main = do
--  let seq = C'CvSeq 1 10
--                    nullPtr nullPtr nullPtr nullPtr
--                    111
--                    12
--                    nullPtr
--                    nullPtr
--                    222
--                    nullPtr
--                    nullPtr
--
--  with seq c'printSeq
--  with seq $ \sp -> peek sp >>= print
