{-#LANGUAGE ParallelListComp,ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.ImageOp
import CV.Drawing
import CV.ColourUtils

main = do
    Just x <- loadColorImage "smallLena.jpg"
    let y :: Image LAB D32
        y = rgbToLab x
    saveImage "channels.png" $ montage (3,2) 5 $
        [getChannel Red x   <# putTextOp 1 1 "Red" (150,190)
        ,getChannel Green x <# putTextOp 1 1 "Green" (150,190)
        ,getChannel Blue x  <# putTextOp 1 1 "Blue" (150,190)
        ,(stretchHistogram $ getChannel LAB_L y) <# putTextOp 0 1.2 "LAB:L" (150,190)
        ,(stretchHistogram $ getChannel LAB_A y) <# putTextOp 0 1.2 "LAB:A" (150,190)
        ,(stretchHistogram $ getChannel LAB_B y) <# putTextOp 0 1.2 "LAB:B" (150,190)
        ]

