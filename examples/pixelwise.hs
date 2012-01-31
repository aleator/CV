module Main where
import CV.Image
import CV.Edges
import qualified CV.ImageMath as IM
import CV.ColourUtils
import CV.Pixelwise
import CV.Filters
import qualified CV.Transforms as T
import Control.Applicative hiding ((<**>))

main = do
    Just x <- loadImage "smallLena.jpg"
    let  y = fromImage $ T.flip T.Horizontal x
         u = fromImage $ T.flip T.Vertical   y
         z = fromImage $ T.flip T.Vertical   x
    saveImage "PixelWise.png" $ montage (3,3) 5 
        [x,y
        ,stretchHistogram . toImage $ (z + y)
        ,stretchHistogram . toImage $ y + u + z
        ,stretchHistogram . toImage $ fmap (sin . (*9)) $ y 
        ,stretchHistogram . toImage $ fmap log $ x 
        
        ,stretchHistogram . gaussian (3,3) 
                          . toImage $ atan2 <$$> (sobel (1,0) s5 x) 
                                            <+>  (sobel (0,1) s5 x) 
        ,toImage $ fmap (\x -> if x > 0.5 then 0 else 1) $ x
        ,toImage $ fmap (\x -> if x > 0.5 && x < 0.6 then 0 else 1)  $ x
        ]
