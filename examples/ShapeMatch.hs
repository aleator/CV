{-#LANGUAGE ParallelListComp#-}
module ShapeMatch where
import CV.Image
import CV.TemplateMatching
import qualified CV.ImageMath as IM
import System.Directory
import Control.Applicative
import Data.Maybe
import Control.Monad
import Data.List
import CV.Drawing
import CV.Transforms
import CV.ImageOp


main = do
     files <- filter (".png" `isSuffixOf`) <$> getDirectoryContents "shapes/"
     images <- mapM (loadImage.("shapes/"++) >=> (return.fromJust)) files
     Just target <- loadImage "shapePhoto.jpg"
     let ops :: Image GrayScale D32
         ops = target <## 
                [let ((x,y),v) = simpleTemplateMatch CV_TM_CCORR_NORMED 
                                                     (target) (scaleToSize Cubic False (sx,sy) img)
                in putTextOp 1 1 fnx (x,y) #> rectOpS 1 1 (x,y) (sx,sy)
                | img <- images | fnx <- files ]
         (sx,sy) = (87, 65)
     saveImage "shapes.png" ops
