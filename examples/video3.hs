{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Video
import Utils.Stream

main = do
    Just x <- loadImage "smallLena.jpg"
    print "finding capture"
    Just cap <- captureFromCam (-1)
    print "capture acquired"
   -- Just f <- getFrame cap
   --saveImage "video.png" $ f
    imgs :: [Image RGB D32] <- runStream . sideEffect (\_ -> print "frame taken") 
                                         . takeS (6*6) 
                                         $ streamFromVideo cap
    print (map getSize imgs)
    saveImage "video.png"  $ montage (6,6) 2 (imgs)

