module Main where
import Criterion.Main 
import CV.Image
import qualified CV.ImageMath as IM
import System.IO.Unsafe
import CV.ColourUtils
import CV.Pixelwise
import qualified CV.Transforms as T

pwFlip :: Image GrayScale D32 -> Image GrayScale D32
pwFlip i = unsafePerformIO $ withClone i (return . toImage . remap flip . fromImage)
    where flip f = \(x,y) -> f (w-x,y) 
          (w,h)  = getSize i 

main = do
        Just x <-  loadImage "smallLena.jpg" 
        Just xc <- loadColorImage"smallLena.jpg" 
        print ("getpixel", getPixelOld (100,100) x,getPixel (100,100) x)
        print ("RGB",getPixelOldRGB (100,100) xc,getPixel (100,100) xc)
        let nAtan :: Image GrayScale D32 ->  Image GrayScale D32 
            nAtan x = unsafePerformIO $ withClone x (mapImageInplace atan)
            nSqrt :: Image GrayScale D32 ->  Image GrayScale D32 
            nSqrt x = unsafePerformIO $ withClone x (mapImageInplace sqrt)
            nSqrtPw :: Image GrayScale D32 ->  Image GrayScale D32 
            nSqrtPw x = unsafePerformIO $ withClone x (return . toImage . fmap sqrt . fromImage)
            nSqrtPP :: Image GrayScale D32 ->  Image GrayScale D32 
            nSqrtPP x = unsafePerformIO $ withClone x (return . toImageP . fmap sqrt . fromImage)

        
        print ("Atan-eq", getPixel (100,100) (IM.atan x), getPixel (100,100) (nAtan x))

        saveImage "A.png" ( nSqrt $ x)
        saveImage "B.png" ( IM.sqrt $ x)
        let gp = getPixel (100,100)

        defaultMain [
         bgroup "getPixel" [
               bench "old" $ nf ((flip getPixelOld) x :: (Int,Int) -> Float) (105,105)
             , bench "new" $ nf ((flip getPixel)    x) (105,105) 
             , bench "old3" $ nf ((flip getPixelOldRGB) xc :: (Int,Int) -> (Float,Float,Float)) (105,105)
             , bench "new3" $ nf ((flip getPixel)    xc) (105,105)]
         
         ,bgroup "setPixel" [
               bench "Old" $ setPixelOld (105,105) 1 x 
             , bench "new" $ setPixelOld (105,105) 1 x 
                            ]

         ,bgroup "transformations" [
               bench "Old" $ nf (T.flip T.Horizontal) x
             , bench "new" $ nf pwFlip x
                            ]
         ,bgroup "sqrt" [    
               bench "im-sqrt"      $ saveImage "sqrt-im.png" $ (IM.sqrt) x
             , bench "map-sqrt"      $ saveImage "sqrt-m.png" $ (nSqrt) x
             , bench "fmap-sqrt"     $ saveImage "sqrt-f.png" $ (nSqrtPw) x
             , bench "p-fmap-sqrt"   $ saveImage "sqrt-p.png" $ (nSqrtPP) x
             ] 

         ,bgroup "mapping" [    
               bench "map-atan"  $ nf (gp . nAtan) x
             , bench "im-atan"   $ nf (gp . IM.atan) x
             , bench "map-sqrt"  $ nf (gp . nSqrt) x
             , bench "im-sqrt"   $ nf (gp . IM.sqrt) x
             ] ]

