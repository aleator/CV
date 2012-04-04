{-#LANGUAGE ParallelListComp#-}
module Main where

import CV.Image
import CV.Tracking
import CV.Bindings.Types
import CV.Drawing
import CV.ImageOp
import CV.Transforms
import Utils.Rectangle
import Text.Printf

main' = do
   Just x <- loadImage "meanshiftsample.png"
   let start x = Rectangle x 200 60 60
   let res = x <## [rectOp 1 1 y | p <- [0,30..640]
                                 , let (val,y) = meanShift x (start p) (EPS 1) ]
   saveImage "meanshift_result.png" res

mstrack ip (imgs) = foldl (\(bb:bbs) i -> snd (meanShift i bb (EPS 1)):bb:bbs) ([ip]) imgs

main = do
   Just x <- loadImage "meanshiftsample2.png"
   let start = Rectangle 30 30 50 50
       rotations x= [rotate p x | p <- [0,0.1..2*pi-0.1]]
       res = [img <## [rectOp 1 1 box1, rectOp 0.5 2 box2]
             | box1 <- reverse (mstrack (Rectangle 30 30 50 50) (rotations x))
             | box2 <- reverse (mstrack (Rectangle 230 120 50 50) (rotations x))
             | img <- rotations x]
   sequence_ [saveImage (printf "meanshift_result_%.2d_.png" i) r
             | (r,i) <- zip res [1::Int ..]]
