module Main where
import CV.Image
import CV.ColourUtils
import CV.TemplateMatching
import qualified CV.Transforms as T
import CV.ImageOp
import CV.Drawing
import qualified CV.ImageMath as IM

main = do
    Just t <- loadImage "smallLena.jpg"
    Just x <- loadImage "smallLena.jpg"
    let r = (78,80)
        y = getRegion r (16,16) x
        mt = CCOEFF_NORMED

        ((mx,my),d) = simpleTemplateMatch mt t y
    saveImage "templateMatching.png" $ montage (3,1) 5 $
        [x<# rectOpS 0 2 r (26,26) <# putTextOp 0 0.9 "Find this" (95,132)
        ,stretchHistogram $ matchTemplate mt t y 
        ,t <# rectOpS 0 2 (mx,my) (26,26)<# putTextOp 0 0.9 "Found" (mx,my + 37)
        ]


