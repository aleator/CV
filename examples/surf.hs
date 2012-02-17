module Main where
import CV.Image
import CV.Features
import CV.Drawing
import CV.ImageOp
import CV.Bindings.Types
import CV.Transforms
import Utils.GeometryClass
import System.Environment

main = do
   Just x <- getArgs >>= loadImage . head
   let y = rotate (pi/2) x
       lst  = getSURF defaultSURFParams (unsafeImageTo8Bit x) Nothing
       lsty = getSURF defaultSURFParams (unsafeImageTo8Bit y) Nothing
   let result lst x = x <## [ellipseBoxOp 1 (C'CvBox2D c size d) 1 0
                      | (C'CvSURFPoint c l s d h,_) <- lst
                      , let size = C'CvSize2D32f (fromIntegral s) (fromIntegral s)
                      ]
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
