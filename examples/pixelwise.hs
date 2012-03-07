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

main = do
    Just x <- loadImage "smallLena.jpg"
    let  y, u, z :: Image GrayScale D32
         y = T.flip T.Horizontal x
         u = T.flip T.Vertical   y
         z = T.flip T.Vertical   x
         d :: Pixelwise (Complex D32)
         d = fromImage $ dft x
    saveImage "PixelWise.png" $ montage (2,5) 5
        [x,y
        ,stretchHistogram . toImage $ (fromImage z + fromImage y)
        ,stretchHistogram . toImage $ fromImage y + fromImage u + fromImage z
        ,stretchHistogram . toImage $ fmap (sin . (*9)) $ fromImage y
        ,stretchHistogram . toImage $ fmap log $ fromImage x

        ,stretchHistogram . gaussian (3,3)
                          . toImage $ atan2 <$$> (sobel (1,0) s5 x)
                                            <+>  (sobel (0,1) s5 x)
        ,toImage $ fmap (\x -> if x > 0.5 then 0 else 1) $ fromImage x
        ,toImage $ fmap (\x -> if x > 0.5 && x < 0.6 then 0 else 1)  $ fromImage x
        ,logarithmicCompression $ fst $ dftSplit $ (toImage d) <# circleOp (0:+0) (60,60) 30 Filled -- * (fromFunction (getSize d) (\(x,y) -> (exp(-(fromIntegral (91-x)^2+fromIntegral (91-y)^2)/(1600)):+0)))
        ]
