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
    let  y = T.flip T.Horizontal x
         u = T.flip T.Vertical   y
         z = T.flip T.Vertical   x
    saveImage "PixelWise.png" $ montage (3,3) 5 
        [x
        ,y
        ,{-#SCC "+" #-} stretchHistogram . toImage $ ((+) <$$> x <+> y)
        
        ,{-#SCC "++++" #-} stretchHistogram . toImage $ ((\a b c d -> a+b+c+d) <$$> x 
                                                           <+> y
                                                           <+> u
                                                           <+> z)
        ,{-#SCC "sin" #-}stretchHistogram . toImage $ fmap (sin . (*9)) . fromImage $ x 
        ,stretchHistogram . toImage $ fmap log . fromImage $ x 
        
        ,stretchHistogram . gaussian (3,3) 
                          . toImage $ atan2 <$$> fromImage (sobel (1,0) s5 x) 
                                            <+> fromImage (sobel (0,1) s5 x) 
        ,toImage $ fmap (\x -> if x > 0.5 then 0 else 1) . fromImage $ x
        ,toImage $ fmap (\x -> if x > 0.5 && x < 0.6 then 0 else 1) . fromImage $ x
        ]
