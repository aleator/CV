module Main where
import CV.Image

main = do
    x1 <- readFromFile "smallLena.jpg" :: IO (Image GrayScale D32)
    x2 <- readFromFile "smallLena.jpg" :: IO (Image GrayScale D8)
    x3 <- readFromFile "smallLena.jpg" :: IO (Image RGB D32)
    x4 <- readFromFile "smallLena.jpg" :: IO (Image RGB D8)
    saveImage "x1.png" x1
    saveImage "x2.png" x2
    saveImage "x3.png" x3
    saveImage "x4.png" x4
