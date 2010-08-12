module Main where
import CV.Image
import CV.Video

main = do
    cap <- captureFromCam (0)
    sequence_ [print i >> getFrame cap >>= saveImage (show i ++ ".png") | i<-[1..300]]
