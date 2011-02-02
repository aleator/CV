{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, FlexibleContexts#-}
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

logOp :: ImageOperation GrayScale D32
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
                    res <- create (w,h) 
                    withImage i $ \s -> 
                     withImage res $ \r -> do
                      {#call calculateAtan#} s r
                      return res
          

-- Operation that subtracts image mean from image
subtractMeanAbsOp = ImgOp $ \image -> do
                      av <- average' image
                      withGenImage image $ \i -> 
                        {#call wrapAbsDiffS#} i (realToFrac av) i -- TODO: check C datatype sizes

-- Logical inversion of image (Ie. invert, but stay on [0..1] range)
invert i = addS 1 $ mulS (-1) i

absOp = ImgOp $ \image -> do
                      withGenImage image $ \i -> 
                        {#call wrapAbsDiffS#} i 0 i

abs = unsafeOperate absOp

subtractMeanOp :: ImageOperation GrayScale D32
subtractMeanOp = ImgOp $ \image -> do
                      let s =  CV.ImageMath.sum image
                      let mean = s / (fromIntegral $ getArea image )
                      let (ImgOp subop) = subRSOp mean
                      subop image

subRSOp :: D32 -> ImageOperation GrayScale D32
subRSOp scalar =  ImgOp $ \a ->  
          withGenImage a $ \ia -> do
            {#call wrapSubRS#} ia (realToFrac scalar) ia 

subRS s a= unsafeOperate (subRSOp s) a

subSOp scalar =  ImgOp $ \a -> 
          withGenImage a $ \ia -> do
            {#call wrapSubS#} ia (realToFrac scalar) ia 

subS a s = unsafeOperate (subSOp s) a

-- Multiply the image with scalar 
mulSOp :: D32 -> ImageOperation GrayScale D32
mulSOp scalar = ImgOp $ \a ->   
          withGenImage a $ \ia -> do
            {#call cvConvertScale#} ia ia s 0 
            return ()
        where s = realToFrac scalar 
                -- I've heard this will lose information
mulS s = unsafeOperate $ mulSOp s

mkImgScalarOp op scalar = ImgOp $ \a ->   
              withGenImage a $ \ia -> do
                op ia (realToFrac scalar) ia 
                return ()
           -- where s = realToFrac scalar 
                -- I've heard this will lose information

-- TODO: Relax the addition so it works on multiple image depths
addSOp :: D32 -> ImageOperation GrayScale D32
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

-- TODO: For some reason the below was going through 8U images. Investigate
mkCmpOp :: CInt -> D32 -> (Image GrayScale D32 -> Image GrayScale D8)
mkCmpOp cmp = \scalar a -> unsafePerformIO $ do
          withGenImage a $ \ia -> do
                        new  <- create (getSize a) --8UC1
                        withGenImage new $ \cl -> do
                            {#call cvCmpS#} ia (realToFrac scalar) cl cmp
                            --imageTo32F new
                            return new

-- TODO: For some reason the below was going through 8U images. Investigate
mkCmp2Op :: (CreateImage (Image GrayScale d)) => 
           CInt -> (Image GrayScale d -> Image GrayScale d -> Image GrayScale D8)
mkCmp2Op cmp = \imgA imgB -> unsafePerformIO $ do
          withGenImage imgA $ \ia -> do
          withGenImage imgB $ \ib -> do
                        new  <- create (getSize imgA) -- 8U
                        withGenImage new $ \cl -> do
                            {#call cvCmp#} ia ib cl cmp
                            return new
                            --imageTo32F new

-- Compare Image to Scalar
lessThan, moreThan ::  D32 -> Image GrayScale D32 ->Image GrayScale D8

lessThan = mkCmpOp cmpLT
moreThan = mkCmpOp cmpGT

less2Than,lessEq2Than,more2Than :: (CreateImage (Image GrayScale d)) => Image GrayScale d 
                                    -> Image GrayScale d -> Image GrayScale D8 
less2Than = mkCmp2Op cmpLT
lessEq2Than = mkCmp2Op cmpLE
more2Than = mkCmp2Op cmpGT

-- Statistics
average' :: Image GrayScale D32 -> IO D32
average' img = withGenImage img $ \image -> -- TODO: Check c datatype size
                {#call wrapAvg#} image >>= return . realToFrac 

average :: Image GrayScale D32 -> D32
average = realToFrac.unsafePerformIO.average'

sum :: Image GrayScale D32 -> D32
sum img = realToFrac $ unsafePerformIO $ withGenImage img $ \image ->
                    {#call wrapSum#} image

averageImages is = ( (1/(fromIntegral $ length is)) `mulS`) (foldl1 add is)

-- sum img = unsafePerformIO $ withGenImage img $ \image ->
--                    {#call wrapSum#} image

stdDeviation' img = withGenImage img {#call wrapStdDev#} 
stdDeviation :: Image GrayScale D32 -> D32
stdDeviation = realToFrac . unsafePerformIO . stdDeviation' 

stdDeviationMask img mask = unsafePerformIO $ 
                                 withGenImage img $ \i ->
                                  withGenImage mask $ \m ->
                                   {#call wrapStdDevMask#} i m

averageMask img mask = unsafePerformIO $ 
                                 withGenImage img $ \i ->
                                  withGenImage mask $ \m ->
                                   {#call wrapStdDevMask#} i m


{#fun wrapMinMax as findMinMax' 
    { withGenBareImage* `BareImage'
    , withGenBareImage* `BareImage'
    , alloca-  `D32' peekFloatConv*
    , alloca-  `D32' peekFloatConv*} -- TODO: Check datatype sizes used in C!
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

findMinMax i = unsafePerformIO $ do
               nullp <- newForeignPtr_ nullPtr
               (findMinMax' (unS i) (BareImage nullp)) 

-- |Find minimum and maximum value of image i in area specified by the mask.
findMinMaxMask i mask  = unsafePerformIO (findMinMax' i mask) 
-- let a = getAllPixels i in (minimum a,maximum a)

maxValue,minValue :: Image GrayScale D32 -> D32
maxValue = snd.findMinMax
minValue = fst.findMinMax

-- | Render image of 2D gaussian curve with standard deviation of (stdX,stdY) to image size (w,h)
--   The origin/center of curve is in center of the image
gaussianImage :: (Int,Int) -> (Double,Double) -> Image GrayScale D32
gaussianImage (w,h) (stdX,stdY) = unsafePerformIO $ do
    dst <- create (w,h) -- 32F_C1
    withImage dst $ \d-> do
                           {#call render_gaussian#} d (realToFrac stdX) (realToFrac stdY)
                           return dst

-- | Produce white image with 'edgeW' amount of edges fading to black
fadedEdgeImage (w,h) edgeW = unsafePerformIO $ creatingImage ({#call fadedEdges#} w h edgeW)

-- | Produce image where pixel is coloured according to distance from the edge
fadeToCenter (w,h) = unsafePerformIO $ creatingImage ({#call rectangularDistance#} w h )

-- | Merge two images according to a mask. Result R is R = A*m+B*(m-1) .
maskedMerge :: Image GrayScale D8 -> Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
maskedMerge mask img img2 = unsafePerformIO $ do
                              res <- create (getSize img)  -- 32FC1
                              withImage img $ \cimg ->
                               withImage img2 $ \cimg2 ->
                                withImage res $ \cres ->
                                  withImage mask $ \cmask ->
                                   {#call masked_merge#} cimg cmask cimg2 cres
                              return res



