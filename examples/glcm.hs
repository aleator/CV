module Main where

import CV.Image
import qualified Data.Vector as V
import CV.Pixelwise
import CV.ColourUtils


main = do
   Just x <- loadImage "smallLena.jpg" >>= return . fmap unsafeImageTo8Bit
   let
       -- Avustimet
       get (i,j)  = fromIntegral $ getPixel (i,j) x -- Lyhennemerkintä.
       (w,h) = getSize x --koko

       (dx,dy) = (2,2) -- DX/DY on aina positiivinen, sillä talletetaan vain alakolmio.
       toIdx (i,j) =  (max i j*(max i j+1)) `div` 2 + min i j -- Talletetaan vain alakolmiomatriisi

       hist = V.accum (+) (V.replicate (toIdx (255,255)) 0) $ -- Raaka-akkumulaatio
               [(toIdx (get (i,j), get (i+dx,j+dy)),1)
               | i <- [0..w-1-dx]
               , j <- [0..h-1-dy]]
   saveImage "GLCM.png" . logarithmicCompression $ imageFromFunction (255,255) ((V.!) hist . toIdx)
