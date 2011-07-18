{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Video
import Utils.Stream

main = do
    print "finding capture"
    Just cap <- captureFromCam (-1)
    print "capture acquired"
    let frameSize = getFrameSize cap
    writer <- createVideoWriter "test.mpg" MPG4 24 frameSize False
    imgs :: [Image RGB D32] <- runStream . sideEffect (\i -> writeFrame writer (unsafeImageTo8Bit i >> print "frame taken") 
                                         . takeS (6*6) 
                                         $ streamFromVideo cap
    print (map getSize imgs)
    saveImage "video.png"  $ montage (length imgs,1) 2 (imgs)

