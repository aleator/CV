{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.ImageMath where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.Image 
import CV.ImageOp

-- import C2HSTools
{#import CV.Image#}
import Foreign.Marshal
import Foreign.Ptr
import System.IO.Unsafe

import C2HS

mkBinaryImageOpIO f = \a -> \b -> 
          withGenImage a $ \ia -> 
          withGenImage b $ \ib ->
          withClone a    $ \clone ->
          withGenImage clone $ \cl -> do
            f ia ib cl 
            return clone
 
mkBinaryImageOp f = \a -> \b -> unsafePerformIO $
          withGenImage a $ \ia -> 
          withGenImage b $ \ib ->
          withClone a    $ \clone ->
          withGenImage clone $ \cl -> do
            f ia ib cl 
            return clone


-- I just can't think of a proper name for this
   -- Friday Evening
abcNullPtr f = \a b c -> f a b c nullPtr
addOp imageToBeAdded = ImgOp $ \target ->
              withGenImage target $ \ctarget -> 
              withGenImage imageToBeAdded $ \cadd ->
               {#call cvAdd#} ctarget cadd ctarget nullPtr

add = mkBinaryImageOp $ abcNullPtr {#call cvAdd#}
sub = mkBinaryImageOp $ abcNullPtr {#call cvSub#}
subFrom what = ImgOp $ \from ->
          withGenImage from $ \ifrom -> 
          withGenImage what $ \iwhat ->
           {#call cvSub#} ifrom iwhat ifrom nullPtr

logOp  = ImgOp $ \i -> withGenImage i (\img -> {#call cvLog#}  img img)
log = unsafeOperate logOp

sqrtOp  = ImgOp $ \i -> withGenImage i (\img -> {#call sqrtImage#}  img img)
sqrt = unsafeOperate sqrtOp

limitToOp what = ImgOp $ \from ->
          withGenImage from $ \ifrom -> 
          withGenImage what $ \iwhat ->
           {#call cvMin#} ifrom iwhat ifrom

limitTo x y = unsafeOperate (limitToOp x) y 

mul = mkBinaryImageOp 
    (\a b c -> {#call cvMul#} a b c 1)

div = mkBinaryImageOp 
    (\a b c -> {#call cvDiv#} a b c 1)

min = mkBinaryImageOp {#call cvMin#}

max = mkBinaryImageOp {#call cvMax#}

absDiff = mkBinaryImageOp {#call cvAbsDiff#}

atan i = unsafePerformIO $ do
                    let (w,h) = getSize i
                    res <- createImage32F (w,h) 1
                    withImage i $ \s -> 
                     withImage res $ \r -> do
                      {#call calculateAtan#} s r
                      return res
          

-- Operation that subtracts image mean from image
subtractMeanAbsOp = ImgOp $ \image -> do
                      av <- average' image
                      withGenImage image $ \i -> 
                        {#call wrapAbsDiffS#} i av i

-- Logical inversion of image (Ie. invert, but stay on [0..1] range)
invert i = addS 1 $ mulS (-1) i

absOp = ImgOp $ \image -> do
                      withGenImage image $ \i -> 
                        {#call wrapAbsDiffS#} i 0 i

abs = unsafeOperate absOp

subtractMeanOp = ImgOp $ \image -> do
                      let s =  CV.ImageMath.sum image
                      let mean = s / (fromIntegral $ uncurry (*) $ getSize image )
                      let (ImgOp subop) = subRSOp mean
                      subop image

subRSOp scalar =  ImgOp $ \a ->  
          withGenImage a $ \ia -> do
            {#call wrapSubRS#} ia scalar ia 
subRS s a= unsafeOperate (subRSOp s) a

subSOp scalar =  ImgOp $ \a -> 
          withGenImage a $ \ia -> do
            {#call wrapSubS#} ia scalar ia 

subS a s = unsafeOperate (subSOp s) a

-- Multiply the image with scalar 
mulSOp :: Double -> ImageOperation
mulSOp scalar = ImgOp $ \a ->   
          withGenImage a $ \ia -> do
            {#call cvConvertScale#} ia ia s 0 
            return ()
        where s = realToFrac scalar 
                -- I've heard this will lose information
mulS s = unsafeOperate $ mulSOp s

mkImgScalarOp op scalar = ImgOp $ \a ->   
              withGenImage a $ \ia -> do
                op ia scalar ia 
                return ()
           -- where s = realToFrac scalar 
                -- I've heard this will lose information

addSOp = mkImgScalarOp $ {#call wrapAddS#}
addS s = unsafeOperate $ addSOp s

minSOp = mkImgScalarOp $ {#call cvMinS#} 
minS s = unsafeOperate $ minSOp s

maxSOp = mkImgScalarOp $ {#call cvMaxS#}
maxS s = unsafeOperate $ maxSOp s


-- Comparison operators
cmpEQ = 0
cmpGT = 1
cmpGE = 2
cmpLT = 3
cmpLE = 4
cmpNE = 5

mkCmpOp cmp = \scalar a -> unsafePerformIO $ do
          withGenImage a $ \ia -> do
                        new  <- createImage8U (getSize a) 1
                        withGenImage new $ \cl -> do
                            {#call cvCmpS#} ia scalar cl cmp
                            imageTo32F new

mkCmp2Op cmp = \imgA imgB -> unsafePerformIO $ do
          withGenImage imgA $ \ia -> do
          withGenImage imgB $ \ib -> do
                        new  <- createImage8U (getSize imgA) 1
                        withGenImage new $ \cl -> do
                            {#call cvCmp#} ia ib cl cmp
                            imageTo32F new

-- Is image less than a scalar at all points?
lessThan = mkCmpOp cmpLT
moreThan = mkCmpOp cmpGT

-- Is image less than another image
less2Than = mkCmp2Op cmpLT
lessEq2Than = mkCmp2Op cmpLE
more2Than = mkCmp2Op cmpGT

-- Statistics
average' :: Image -> IO CDouble
average' img = withGenImage img $ \image ->
                {#call wrapAvg#} image
average = unsafePerformIO.average'

sum img = unsafePerformIO $ withGenImage img $ \image ->
                    {#call wrapSum#} image

averageImages is = ( (1/(fromIntegral $ length is)) `mulS`) (foldl1 add is)

-- sum img = unsafePerformIO $ withGenImage img $ \image ->
--                    {#call wrapSum#} image

stdDeviation' img = withGenImage img {#call wrapStdDev#} 
stdDeviation img = unsafePerformIO $ stdDeviation' img

stdDeviationMask img mask = unsafePerformIO $ 
                                 withGenImage img $ \i ->
                                  withGenImage mask $ \m ->
                                   {#call wrapStdDevMask#} i m

averageMask img mask = unsafePerformIO $ 
                                 withGenImage img $ \i ->
                                  withGenImage mask $ \m ->
                                   {#call wrapStdDevMask#} i m


{#fun wrapMinMax as findMinMax' 
    { withGenImage* `Image'
    , withGenImage* `Image'
    , alloca-  `Double' peekFloatConv*
    , alloca-  `Double' peekFloatConv*}
    -> `()'#}

findMinMaxLoc img = unsafePerformIO $ 
	     alloca $ \(ptrintmaxx :: Ptr CInt)->
	      alloca $ \(ptrintmaxy :: Ptr CInt)->
           alloca $ \(ptrintminx :: Ptr CInt)->
            alloca $ \(ptrintminy :: Ptr CInt)->
             alloca $ \(ptrintmin :: Ptr CDouble)->
              alloca $ \(ptrintmax :: Ptr CDouble)->
               withImage img $ \cimg -> do {
                 {#call wrapMinMaxLoc#} cimg ptrintminx ptrintminy ptrintmaxx ptrintmaxy ptrintmin ptrintmax;
		         minx <- peek ptrintminx;
		         miny <- peek ptrintminy;
		         maxx <- peek ptrintmaxx;
		         maxy <- peek ptrintmaxy;
		         maxval <- peek ptrintmax;
		         minval <- peek ptrintmin;
                 return (((minx,miny),minval),((maxx,maxy),maxval));}

-- I've got no clue what is supposed to happen below, but I hope it doesn't work..
-- |DEPRECATED
findMinMax i = unsafePerformIO $ do
               nullp <- newForeignPtr_ nullPtr
               (findMinMax' i (Image nullp)) 

-- |Find minimum and maximum value of image i in area specified by the mask.
findMinMaxMask i mask  = unsafePerformIO (findMinMax' i mask) 
-- let a = getAllPixels i in (minimum a,maximum a)

maxValue = snd.findMinMax
minValue = fst.findMinMax 

-- | Render image of 2D gaussian curve with standard deviation of (stdX,stdY) to image size (w,h)
--   The origin/center of curve is in center of the image
gaussianImage (w,h) (stdX,stdY) = unsafePerformIO $ 
    let dst = image32F (w,h) 1
    in withImage dst $ \d-> do
                             {#call render_gaussian#} d (realToFrac stdX) (realToFrac stdY)
                             return dst

-- | Produce white image with 'edgeW' amount of edges fading to black
fadedEdgeImage (w,h) edgeW = unsafePerformIO $ creatingImage ({#call fadedEdges#} w h edgeW)

-- | Produce image where pixel is coloured according to distance from the edge
fadeToCenter (w,h) = unsafePerformIO $ creatingImage ({#call rectangularDistance#} w h )

-- | Merge two images according to a mask. Result R is R = A*m+B*(m-1) .
maskedMerge mask img img2 = unsafePerformIO $ do
                              let res = image32F (getSize img) 1 
                              withImage img $ \cimg ->
                               withImage img2 $ \cimg2 ->
                                withImage res $ \cres ->
                                  withImage mask $ \cmask ->
                                   {#call masked_merge#} cimg cmask cimg2 cres
                              return res



