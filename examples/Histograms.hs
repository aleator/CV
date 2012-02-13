module Main where
import CV.Image
import CV.Histogram as H
import CV.ColourUtils

main = do
    Just full <- loadColorImage "building.jpg"
    Just sample <- loadColorImage "building_sample2.jpg"
    let hist = H.Histogram [(unsafeImageTo8Bit $ getChannel Red   sample,64)
                             ,(unsafeImageTo8Bit $ getChannel Green sample,64)
                             ,(unsafeImageTo8Bit $ getChannel Blue  sample,64)] False Nothing 
        res  = backProjectHistogram [unsafeImageTo8Bit $ getChannel Red full
                                    ,unsafeImageTo8Bit $ getChannel Green full
                                    ,unsafeImageTo8Bit $ getChannel Blue full] hist 
    saveImage "backproject_result.png" $ stretchHistogram $ unsafeImageTo32F res
