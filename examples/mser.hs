module Main where
import CV.Image
import CV.Features
import CV.Drawing
import CV.ImageOp
import CV.Bindings.Types
import CV.Transforms
import Utils.GeometryClass
import Utils.Point
import System.Environment 

main = do
   Just x <- getArgs >>= loadImage . head 
   let y   = rotate (pi/4) x
       lst  = getMSER (unsafeImageTo8Bit x) Nothing defaultMSERParams
       lsty = getMSER (unsafeImageTo8Bit y) Nothing defaultMSERParams
       result lst x = x <## [drawLinesOp 1 1 $ polyline ctr
                            | ctr <- lst]
   print lst
   saveImage "mser.png" $ montage (2,1) 2 [result (take 100 lst) x ,result (take 100 lsty) y]

polyline pts = pts `zip` tail pts

