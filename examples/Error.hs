module Main where
import CV.Image
import Prelude hiding (catch)
import CV.Transforms
import CV.ImageMathOp
import Control.Exception

main = do
   setCatch
   Just x <- loadImage "smallLena.jpg"
   let y = scale Linear (2,2) x
   saveImage "poks.ok" (x #+ y)
    `catch` (\e -> do let err = show (e :: CvException)
                      putStr (show e)
                      return ())
   print "done"

