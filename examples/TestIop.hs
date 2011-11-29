module Main where
import CV.Image
import CV.ImageOp
import qualified CV.Filters as F
import CV.ImageMath
import Foreign.Ptr
import CV.ColourUtils
import Prelude hiding (subtract, (.), id)
import qualified CV.ImageMath as IM
import Control.Category
idOp = IOP return

subtract = IOP $ \(a,b) -> do
                withGenImage a $ \ca -> 
                 withGenImage b $ \cb -> cvSub ca cb ca nullPtr >> return a

subtractScalar sc = IOP $ \a -> do
                  withGenImage a $ \ca -> 
                    wrapSubS ca (realToFrac sc) ca >> return a

mulScalar sc = IOP $ \a -> do
                  withGenImage a $ \ca -> 
                    cvConvertScale ca ca (realToFrac sc) 0 >> return a

liftIOP f = IOP (return.f)

--stretch image = stretched
--            where
--             stretched = (1/realToFrac length) `IM.mulS` normed
--             normed = image `IM.subS` (realToFrac min)
--             length = max-min
--             (min,max) = IM.findMinMax image

ret x = IOP (const (return x))

gaussian s = fromImageOp (F.gaussianOp s)

main = do
    Just x <- loadImage "elaine.jpg"
    let iop = (gaussian (11,11) &#& id) >>> subtract >>> mulScalar 3
    runIOP iop x >>= saveImage "IOPTest.png"
