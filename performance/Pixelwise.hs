module Main where
import Criterion.Main 
import CV.Image
import qualified CV.ImageMath as IM
import System.IO.Unsafe
import CV.ColourUtils

main = do
        Just x <-  loadImage "smallLena.jpg" 
        Just xc <- loadColorImage"smallLena.jpg" 
        print ("getpixel", getPixelOld (100,100) x,getPixel (100,100) x)
        print ("RGB",getPixelOldRGB (100,100) xc,getPixel (100,100) xc)
        let nAtan :: Image GrayScale D32 ->  Image GrayScale D32 
            nAtan x = unsafePerformIO $ withClone x (mapImageInplace atan)
            nSqrt :: Image GrayScale D32 ->  Image GrayScale D32 
            nSqrt x = unsafePerformIO $ withClone x (mapImageInplace sqrt)
        
        print ("Atan-eq", getPixel (100,100) (IM.atan x), getPixel (100,100) (nAtan x))

        saveImage "A.png" ( nSqrt $ x)
        saveImage "B.png" ( IM.sqrt $ x)
        let gp = getPixel (100,100)

        defaultMain [
           bench "oldPixel" $ nf ((flip getPixelOld) x :: (Int,Int) -> Float) (105,105)
         , bench "newPixel" $ nf ((flip getPixel)    x) (105,105) 
         , bench "oldPixel3" $ nf ((flip getPixelOldRGB) xc :: (Int,Int) -> (Float,Float,Float)) (105,105)
         , bench "newPixel3" $ nf ((flip getPixel)    xc) (105,105) 

         , bench "map-atan"  $ nf (gp . nAtan) x
         , bench "im-atan"   $ nf (gp . IM.atan) x
         , bench "map-sqrt"  $ nf (gp . nSqrt) x
         , bench "im-sqrt"   $ nf (gp . IM.sqrt) x
         ]

