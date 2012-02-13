{-#LANGUAGE ScopedTypeVariables#-}
module Main where
import CV.Image
import CV.Video
import CV.HighGUI
import Utils.Stream
import Control.Concurrent

main = do
    print "finding capture"
    Just cap <- captureFromCam (-1)
    print "capture acquired"
    win <- makeWindow "test"
    let callback i = print i >> setCapProp cap CAP_PROP_GAIN (i/100) >> return ()
    mkTrackbar 100 50 "bar" "test" callback
    runStream_ . sideEffect (\i -> showImage "test" i >> waitKey 20>> print "frame taken") 
                                         . takeS (450) 
                                         $ streamFromVideo cap
    waitKey 10
    destroyWindow "test"
