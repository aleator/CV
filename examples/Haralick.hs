module Main where
import CV.Image
import CV.Conversions
import CV.Haralick
import CV.ColourUtils
import System.IO.Unsafe

sidesToMiddle img = blitM (w,h) [((0,0),bottom),((0,mid-1),top)] 
    where
     (w,h) = getSize img
     mid = h `div` 2
     top    = getRegion (0,0) (w, mid)  img
     bottom = getRegion (0,mid+1) (w,h-mid-1) img

analyze fn = do 
    Just x <- loadImage fn
    let co_occ = coOccurenceMatrix (2,2) $ unsafeImageTo8Bit $ stretchHistogram x
        (w,h)  = getSize x
        norm   = fromIntegral $ w*h 
    saveImage (fn++"_haralick.png") $ sidesToMiddle $ logarithmicCompression 
                                    $ unsafeImageTo32F $ copyCArrayToImage co_occ
    contr <- contrast co_occ 256
    ang <- angularSecondMoment co_occ 256
    putStrLn fn
    print (w,h)
    print ("Contrast", contr )
    print ("ASM",       ang)
    putStrLn ""

main = do
   analyze "smallLena.jpg"
   analyze "rynkky4.bmp"
   analyze "rynkky5.bmp"
    
