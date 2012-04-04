{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import CV.ColourUtils
import CV.DFT
import CV.Drawing
import CV.Edges
import CV.Filters
import CV.Image
import CV.ImageOp
import CV.Pixelwise
import Control.Applicative hiding ((<**>))
import qualified CV.ImageMath as IM
import qualified CV.Transforms as T
import Data.Complex

fi = fromIntegral

main = do
    Just x <- loadImage "smallLena.jpg"
    let  d :: Pixelwise (Complex D32)
         d  = fromImage $ dft x
         mpper f = \(x,y) -> f (x,y) + (sin (7*fi x))
         d2' = remapImage mpper x
         d2  = dft d2'
         d2c = d2 <# circleOp (0:+0) (130,102) 13 Filled
                  <# circleOp (0:+0) (71,102)  13 Filled

         dftd1 = (toImage d) <# circleOp (0:+0) (101,101) 30 Filled
    saveImage "dftmanip.png" $ montage (2,3) 10
        [
         logarithmicCompression $ fst $ dftSplit $ dftd1
        ,stretchHistogram $ idft dftd1
        ,stretchHistogram $ d2'
        ,logarithmicCompression $ fst $ dftSplit $ d2
        ,stretchHistogram $ fst $ dftSplit $ d2c
        ,stretchHistogram $ idft $ d2c
        ]
