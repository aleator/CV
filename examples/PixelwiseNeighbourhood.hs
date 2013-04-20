module Main where
import CV.Image
import CV.Pixelwise

main = do
   Just x <- loadImage "smallLena.jpg"
   let r :: Image GrayScale D32
       r= toImage . mapNeighbourhood (\n -> (n (0,-1))-(n (0,1))) . fromImage $ x
   saveImage "PWN.png" $ r
