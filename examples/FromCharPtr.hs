module Main where
import CV.Image
import CV.ColourUtils
import CV.Conversions
import Data.Array.CArray
import Data.Array.IArray
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

main = do
    let values = concat [[129,x,y] | x <- [0..99], y <- [0..99]]
    print $ (length values,99*99*3)
    ptr <- newArray $ values
    let image :: Image RGB D8    
        image = unsafe8UC3FromPtr (100,100) ptr
    print $ getPixel (0,0) image
    print $ getPixel (50,50) image
    print $ getPixel (00,99) image
    saveImage "sin.png" $ image
