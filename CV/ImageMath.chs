{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, FlexibleContexts#-}
#include "cvWrapLEO.h"
module CV.ImageMath where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.Bindings.Types
import CV.Bindings.Core
import CV.Image
import CV.ImageOp

-- import C2HSTools
{#import CV.Image#}
import Foreign.Marshal
import Foreign.Ptr
import System.IO.Unsafe
import Control.Applicative ((<$>))

import C2HS hiding (unsafePerformIO)

mkBinaryImageOpIO f = \a -> \b ->
          withGenImage a $ \ia ->
          withGenImage b $ \ib ->
          withCloneValue a    $ \clone ->
          withGenImage clone $ \cl -> do
            f ia ib cl
            return clone

mkBinaryImageOp
  :: (Ptr () -> Ptr () -> Ptr () -> IO a)
     -> CV.Image.Image c1 d1
     -> CV.Image.Image c1 d1
     -> CV.Image.Image c1 d1

mkBinaryImageOp f = \a -> \b -> unsafePerformIO $
          withGenImage a $ \ia ->
          withGenImage b $ \ib ->
          withCloneValue a    $ \clone ->
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

atan :: Image GrayScale D32 -> Image GrayScale D32
atan i = unsafePerformIO $ do
                    let (w,h) = getSize i
                    res <- create (w,h)
                    withImage i $ \s ->
                     withImage res $ \r -> do
                      {#call calculateAtan#} s r
                      return res

atan2 a b = unsafePerformIO $ do
                    res <- create (getSize a)
                    withImage a $ \c_a ->
                     withImage b $ \c_b ->
                      withImage res $ \c_res -> do
                       {#call calculateAtan2#} c_a c_b c_res
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
                      let (ImgOp subop) = subRSOp (realToFrac mean)
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
average' :: Image GrayScale a -> IO D32
average' img = withGenImage img $ \image -> -- TODO: Check c datatype size
                {#call wrapAvg#} image >>= return . realToFrac

average :: Image GrayScale D32 -> D32
average = realToFrac.unsafePerformIO.average'

-- | Sum the pixels in the image. Notice that OpenCV automatically casts the
--   result to double sum :: Image GrayScale D32 -> D32
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
		         minx <- fromIntegral <$> peek ptrintminx;
		         miny <- fromIntegral <$> peek ptrintminy;
		         maxx <- fromIntegral <$> peek ptrintmaxx;
		         maxy <- fromIntegral <$> peek ptrintmaxy;
		         maxval <- realToFrac <$> peek ptrintmax;
		         minval <- realToFrac <$> peek ptrintmin;
                 return (((minx,miny),minval),((maxx,maxy),maxval));}

imageMinMax i = unsafePerformIO $ do
  withImage i $ \i_ptr -> do
    let
      minval :: CDouble
      minval = 0
      maxval :: CDouble
      maxval = 0
    with minval $ \cminval ->
      with maxval $ \cmaxval -> do
        c'cvMinMaxLoc (castPtr i_ptr) cminval cmaxval nullPtr nullPtr nullPtr
        imin <- peek cminval
        imax <- peek cmaxval
        return ((realToFrac imin), (realToFrac imax))

imageAvgSdv i = unsafePerformIO $ do
  withImage i $ \i_ptr -> do
    let
      avg = (C'CvScalar 0 0 0 0)
      sdv = (C'CvScalar 0 0 0 0)
    with avg $ \avg_ptr ->
      with sdv $ \sdv_ptr -> do
        c'cvAvgSdv (castPtr i_ptr) avg_ptr sdv_ptr nullPtr
        (C'CvScalar a1 a2 a3 a4) <- peek avg_ptr
        (C'CvScalar s1 s2 s3 s4) <- peek sdv_ptr
        return ((realToFrac a1, realToFrac a2, realToFrac a3, realToFrac a4),
                (realToFrac s1, realToFrac s2, realToFrac s3, realToFrac s4))

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
-- TODO: Fix C-code of masked_merge to accept D8 input for the mask
maskedMerge :: Image GrayScale D8 -> Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
maskedMerge mask img img2 = unsafePerformIO $ do
                              res <- create (getSize img)  -- 32FC1
                              withImage img $ \cimg ->
                               withImage img2 $ \cimg2 ->
                                withImage res $ \cres ->
                                  withImage (unsafeImageTo32F mask) $ \cmask ->
                                   {#call masked_merge#} cimg cmask cimg2 cres
                              return res



-- | Given a distance map and a circle, return the biggest circle with radius less
--   than given in the distance map that fully covers the previous one

maximalCoveringCircle distMap (x,y,r)
  = unsafePerformIO $
     withImage distMap $ \c_distmap ->
       alloca $ \(ptr_int_max_x :: Ptr CInt) ->
        alloca $ \(ptr_int_max_y :: Ptr CInt) ->
         alloca $ \(ptr_double_max_r :: Ptr CDouble) ->
          do
           {#call maximal_covering_circle#} x y r c_distmap ptr_int_max_x ptr_int_max_y ptr_double_max_r
           max_x <- fromIntegral <$> peek ptr_int_max_x
           max_y <- fromIntegral <$> peek ptr_int_max_y
           max_r <- realToFrac   <$> peek ptr_double_max_r
           return (max_x,max_y,max_r)




