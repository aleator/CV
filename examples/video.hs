{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Video
import Utils.Stream

main = do
    Just x <- loadImage "smallLena.jpg"
    print "finding capture"
    cap <- captureFromCam (1)
    print "capture acquired"
    imgs :: [Image RGB D32] <- runStream $ sideEffect (\_ -> print "frame taken") $ takeS 36 $ streamFromVideo cap
    saveImage "video.png" $ montage (6,6) 5 $ imgs

