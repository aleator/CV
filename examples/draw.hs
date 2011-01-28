{-#LANGUAGE ParallelListComp,ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Drawing
import CV.ImageOp

main = do
    Just x <- loadColorImage "smallLena.jpg"
    saveImage "lines.png" $ x <## [(lineOp c t (102,102) (x,204)) 
                                  | c <- cycle [(1,0,0),(0,1,0),(0,0,1)] 
                                  | t <- cycle [1,3,5::Int]
                                  | x <- [1,15..204::Int]
                                  ]
                              <# circleOp (0.5,0.6,0.1) (104,104) 30 Filled

