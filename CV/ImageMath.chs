{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, FlexibleContexts#-}
#include "cvWrapLEO.h"

-- | Mathematical and statistical operations for images. See also module
--   "CV.ImageMathOp" which contains handy operators for some of these.
module CV.ImageMath(
  -- * Operations  for two images
  add
, sub
, absDiff
, mul
, CV.ImageMath.div
, CV.ImageMath.min
, CV.ImageMath.max
, maskedMerge
, averageImages
, CV.ImageMath.atan2
  -- * Operations for one image
, subMean
, subMeanAbs
, CV.ImageMath.sqrt
, CV.ImageMath.log
, CV.ImageMath.abs
, CV.ImageMath.atan
, invert
  -- * Operations for a scalar and an image
, addS
, subS
, subRS
, mulS
-- TODO: divS is missing, is it needed?
-- , divS
, minS
, maxS
  -- * Pixelwise logical operations
, CV.ImageMath.not
, CV.ImageMath.and
, CV.ImageMath.or
, CV.ImageMath.notOp
, CV.ImageMath.andOp
, CV.ImageMath.orOp
  -- * Comparison operations
, lessThan
, moreThan
, less2Than
, more2Than
, lessEq2Than 
, moreEq2Than
  -- * Image statistics
, CV.ImageMath.sum
, average
, countNonZero
, averageMask
, stdDeviation
, stdDeviationMask
, findMinMax
, findMinMaxLoc
, findMinMaxMask
, imageMinMax -- TODO: merge with findMinMax / replace binding?
, minValue
, maxValue
, imageAvgSdv -- TODO: merge with average and stdDeviation / replace binding?
  -- * Misc (to be moved?)
, gaussianImage -- TODO: move to other module?
, fadedEdgeImage -- TODO: move to other module?
, fadeToCenter -- TODO: move to other module?
, maximalCoveringCircle -- TODO: move to other module?
, limitToOp -- TODO: remove? needed in Morphology; export operations at end?
-- TODO: add section "Image operations" and add the bare operations there for combining with others?
) where

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
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe
import Control.Applicative ((<$>))

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

-- | Calculates the per-pixel sum of two images.
add = mkBinaryImageOp $ abcNullPtr {#call cvAdd#}

-- | Calculates the per-pixel difference of two images.
sub = mkBinaryImageOp $ abcNullPtr {#call cvSub#}

subFrom what = ImgOp $ \from ->
          withGenImage from $ \ifrom ->
          withGenImage what $ \iwhat ->
           {#call cvSub#} ifrom iwhat ifrom nullPtr

logOp :: ImageOperation GrayScale D32
logOp  = ImgOp $ \i -> withGenImage i (\img -> {#call cvLog#}  img img)

-- | Calculates the natural logarithm of every pixel.
log = unsafeOperate logOp

sqrtOp :: ImageOperation GrayScale D32
sqrtOp  = ImgOp $ \i -> withGenImage i (\img -> {#call sqrtImage#}  img img)

-- | Calculates the square root of every pixel.
sqrt = unsafeOperate sqrtOp

-- | Operation to limit image with another image; same as 'ImageMath.min'.
limitToOp what = ImgOp $ \from ->
          withGenImage from $ \ifrom ->
          withGenImage what $ \iwhat ->
           {#call cvMin#} ifrom iwhat ifrom

-- | Limit image with another image; same as 'ImageMath.min'.
limitTo x y = unsafeOperate (limitToOp x) y

-- | Calculates the per-pixel product of two images.
mul = mkBinaryImageOp
    (\a b c -> {#call cvMul#} a b c 1)

-- | Calculates the per-pixel division of two images.
div = mkBinaryImageOp
    (\a b c -> {#call cvDiv#} a b c 1)

-- | Calculates the per-pixel minimum of two images.
min = mkBinaryImageOp {#call cvMin#}

-- | Calculates the per-pixel maximum of two images.
max = mkBinaryImageOp {#call cvMax#}

-- | Calculates the per-pixel absolute difference of two images.
absDiff = mkBinaryImageOp {#call cvAbsDiff#}

-- | Calculates the atan of every pixel.
atan :: Image GrayScale D32 -> Image GrayScale D32
atan i = unsafePerformIO $ do
                    let (w,h) = getSize i
                    res <- create (w,h)
                    withImage i $ \s ->
                     withImage res $ \r -> do
                      {#call calculateAtan#} s r
                      return res

-- | Calculates the atan2 of pixel values in two images.
atan2 :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
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

-- | Calculates the absolute difference of every pixel to image mean.
--   See also 'ImageMath.subMean'.
subMeanAbs = unsafeOperate subtractMeanAbsOp

-- | Logical inversion of image (ie. invert, but stay on [0..1] range;
--   multiply by @-1@ and add @1@).
invert i = addS 1 $ mulS (-1) i

notOp :: ImageOperation GrayScale D8
notOp =  ImgOp $ \image -> withGenImage image $ \i -> {#call cvNot#} i i

not :: Image GrayScale D8 -> Image GrayScale D8
not = unsafeOperate notOp

andOp :: Image GrayScale D8 -> Mask -> ImageOperation GrayScale D8
andOp image0 mask = ImgOp $ \image1 -> 
                  withGenImage image0 $ \ci0 ->
                  withGenImage image1 $ \ci1 -> 
                  withMask     mask   $ \cmask -> 
                  {#call cvAnd#} ci0 ci1 ci1 cmask

and :: Image GrayScale D8 -> Image GrayScale D8 -> Mask -> Image GrayScale D8 
and i j mask = unsafeOperate (andOp i mask) j

orOp :: Image GrayScale D8 -> Mask -> ImageOperation GrayScale D8
orOp image0 mask = ImgOp $ \image1 -> 
                  withGenImage image0 $ \ci0 ->
                  withGenImage image1 $ \ci1 -> 
                  withMask     mask   $ \cmask -> 
                  {#call cvOr#} ci0 ci1 ci1 cmask

or :: Image GrayScale D8 -> Image GrayScale D8 -> Mask -> Image GrayScale D8 
or i j mask = unsafeOperate (orOp i mask) j

absOp = ImgOp $ \image -> do
                      withGenImage image $ \i ->
                        {#call wrapAbsDiffS#} i 0 i

-- | Calculates the absolute value of every pixel.
abs = unsafeOperate absOp

subtractMeanOp :: ImageOperation GrayScale D32
subtractMeanOp = ImgOp $ \image -> do
                      let s =  CV.ImageMath.sum image
                      let mean = s / (fromIntegral $ getArea image )
                      let (ImgOp subop) = subRSOp (realToFrac mean)
                      subop image

-- | Calculates the (non-absolute) difference of every pixel to image mean.
--   See also 'ImageMath.subMeanAbs'.
subMean = unsafeOperate subtractMeanOp

subRSOp :: D32 -> ImageOperation GrayScale D32
subRSOp scalar =  ImgOp $ \a ->
          withGenImage a $ \ia -> do
            {#call wrapSubRS#} ia (realToFrac scalar) ia

-- | Subtracts a scalar from every pixel, scalar on right.
subRS s a= unsafeOperate (subRSOp s) a

subSOp scalar =  ImgOp $ \a ->
          withGenImage a $ \ia -> do
            {#call wrapSubS#} ia (realToFrac scalar) ia

-- | Subtracts a scalar from every pixel, scalar on left.
subS a s = unsafeOperate (subSOp s) a

-- Multiply the image with scalar
mulSOp :: D32 -> ImageOperation GrayScale D32
mulSOp scalar = ImgOp $ \a ->
          withGenImage a $ \ia -> do
            {#call cvConvertScale#} ia ia s 0
            return ()
        where s = realToFrac scalar
                -- I've heard this will lose information

-- | Multiplies every pixel by a scalar.
mulS s = unsafeOperate $ mulSOp s

mkImgScalarOp op scalar = ImgOp $ \a ->
              withGenImage a $ \ia -> do
                op ia (realToFrac scalar) ia
                return ()

-- TODO: Relax the addition so it works on multiple image depths
addSOp :: D32 -> ImageOperation GrayScale D32
addSOp = mkImgScalarOp $ {#call wrapAddS#}

-- | Adds a scalar to every pixel.
addS s = unsafeOperate $ addSOp s

minSOp = mkImgScalarOp $ {#call cvMinS#}

-- | Calculates the per-pixel minimum between an image and a scalar.
minS :: Float -> Image c d -> Image c d
minS s = unsafeOperate $ minSOp s

maxSOp = mkImgScalarOp $ {#call cvMaxS#}

-- | Calculates the per-pixel maximum between an image and a scalar.
maxS :: Float -> Image c d -> Image c d
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
mkCmpOp cmp = \scalar a -> unsafePerformIO $
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

-- | Compares each pixel to a scalar, and produces a binary image where the
--   pixel value is less than the scalar. For example, @(lessThan s I)@ has
--   white pixels where value of I is less than s. Notice that the order of
--   operands is opposite to the intuitive interpretation of @s ``lessThan`` I@.
lessThan ::  D32 -> Image GrayScale D32 -> Image GrayScale D8
lessThan = mkCmpOp cmpLT

-- | Compares each pixel to a scalar, and produces a binary image where the
--   pixel value is greater than the scalar. For example, @(moreThan s I)@ has
--   white pixels where value of I is greater than s. Notice that the order of
--   operands is opposite to the intuitive interpretation of @s ``moreThan`` I@.
moreThan ::  D32 -> Image GrayScale D32 -> Image GrayScale D8
moreThan = mkCmpOp cmpGT

-- | Compares two images and produces a binary image that has white pixels in
--   those positions where the comparison is true. For example,
--   @(less2Than A B)@ has white pixels where value of A is less than value of
--   B. Notice that these functions follow the intuitive order of operands,
--   unlike 'lessThan' and 'moreThan'.
less2Than,lessEq2Than,more2Than, moreEq2Than
     :: (CreateImage (Image GrayScale d)) => Image GrayScale d -> Image GrayScale d -> Image GrayScale D8

less2Than = mkCmp2Op cmpLT
lessEq2Than = mkCmp2Op cmpLE
moreEq2Than = mkCmp2Op cmpGE
more2Than = mkCmp2Op cmpGT

-- Statistics

average' :: Image GrayScale D32 -> IO D32
average' img = withGenImage img $ \image -> 
                {#call wrapAvg#} image nullPtr >>= return . realToFrac

-- | Calculate number of nonzero pixels
countNonZero :: Image GrayScale a -> Int
countNonZero img = unsafePerformIO $ withGenImage img $ \cImg -> fromIntegral <$> {#call cvCountNonZero#} cImg 



-- | Calculates the average pixel value in whole image.
average :: Image GrayScale D32 -> D32
average = realToFrac.unsafePerformIO.average'

-- | Calculates the average value for pixels that have non-zero mask value.
averageMask :: Image GrayScale D32 -> Image GrayScale D8 -> D32
averageMask img mask = unsafePerformIO $
                       withGenImage img $ \c_image -> 
                       withGenImage mask $ \c_mask -> 
                        {#call wrapAvg#} c_image c_mask >>= return . realToFrac

-- | Calculates the sum of pixel values in whole image
--   (notice that OpenCV automatically casts the result to double).
sum :: Image GrayScale D32 -> D32
sum img = realToFrac $ unsafePerformIO $ withGenImage img $ \image ->
                    {#call wrapSum#} image

-- | Calculates the average of multiple images by adding the pixel values and
--   dividing the resulting values by number of images.
averageImages is = ( (1/(fromIntegral $ length is)) `mulS`) (foldl1 add is)

-- sum img = unsafePerformIO $ withGenImage img $ \image ->
--                    {#call wrapSum#} image

stdDeviation' img = withGenImage img {#call wrapStdDev#}

-- | Calculates the standard deviation of pixel values in whole image.
stdDeviation :: Image GrayScale D32 -> D32
stdDeviation = realToFrac . unsafePerformIO . stdDeviation'

-- | Calculates the standard deviation of values for pixels that have non-zero
--   mask value.
stdDeviationMask img mask = unsafePerformIO $
                                 withGenImage img $ \i ->
                                  withGenImage mask $ \m ->
                                   {#call wrapStdDevMask#} i m


peekFloatConv :: (Storable a, RealFloat a, RealFloat b) => Ptr a -> IO b
peekFloatConv a = fmap realToFrac (peek a)


{#fun wrapMinMax as findMinMax'
    { withGenBareImage* `BareImage'
    , withGenBareImage* `BareImage'
    , alloca-  `D32' peekFloatConv*
    , alloca-  `D32' peekFloatConv*} -- TODO: Check datatype sizes used in C!
    -> `()'#}

-- | Finds the minimum and maximum pixel value in the image and the locations
--   where these values were found.
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

-- TODO: create one function findMinMaxLoc' which takes also mask and base
-- findMinMax, findMinMaxLoc and findMinMaxMask (findMinMaxLocMask?) on it

-- findMinMax and findMinMaxLoc using the new bindings of cvMinMaxLoc...
-- do these really work also with D8 images? depends on fractional instance?
-- maybe create a new class or add a function to some existing class that
-- allows to get a pixel value from float..

-- | Finds the minimum and maximum pixel value in the image.
imageMinMax :: (Fractional d) => Image GrayScale d -> (d,d)
imageMinMax image = unsafePerformIO $ do
  withImage image $ \pimage -> do
    let
      minval :: CDouble
      minval = 0
      maxval :: CDouble
      maxval = 0
    with minval $ \pminval ->
      with maxval $ \pmaxval -> do
        c'cvMinMaxLoc (castPtr pimage) pminval pmaxval nullPtr nullPtr nullPtr
        minv <- peek pminval
        maxv <- peek pmaxval
        return ((realToFrac minv), (realToFrac maxv))

-- | Finds the minimum and maximum pixel value in the image.
imageMinMaxLoc ::  (Fractional d) => Image GrayScale d -> (((Int,Int),d), ((Int,Int),d))
imageMinMaxLoc image = unsafePerformIO $ do
  withImage image $ \pimage -> do
    let
      minval :: CDouble
      minval = 0
      maxval :: CDouble
      maxval = 0
      minloc :: C'CvPoint
      minloc = C'CvPoint 0 0
      maxloc :: C'CvPoint
      maxloc = C'CvPoint 0 0
    with minval $ \pminval ->
      with maxval $ \pmaxval ->
        with minloc $ \pminloc ->
          with maxloc $ \pmaxloc -> do
            c'cvMinMaxLoc (castPtr pimage) pminval pmaxval pminloc pmaxloc nullPtr
            minv <- peek pminval
            (C'CvPoint minx miny) <- peek pminloc
            maxv <- peek pmaxval
            (C'CvPoint maxx maxy) <- peek pmaxloc
            return $
              (((fromIntegral minx, fromIntegral miny), realToFrac minv),
               ((fromIntegral maxx, fromIntegral maxy), realToFrac maxv))

-- TODO: enable using a mask
-- | Calculates the average and standard deviation of pixel values in the image
--   in one operation.
imageAvgSdv :: (Fractional d) => Image GrayScale d -> (d,d)
imageAvgSdv i = unsafePerformIO $ do
  withImage i $ \i_ptr -> do
    let
      avg = (C'CvScalar 0 0 0 0)
      sdv = (C'CvScalar 0 0 0 0)
    with avg $ \avg_ptr ->
      with sdv $ \sdv_ptr -> do
        c'cvAvgSdv (castPtr i_ptr) avg_ptr sdv_ptr nullPtr
        (C'CvScalar a1 _ _ _) <- peek avg_ptr
        (C'CvScalar s1 _ _ _) <- peek sdv_ptr
        return (realToFrac a1, realToFrac s1)

-- | Finds the minimum and maximum pixel value in the image.
findMinMax i = unsafePerformIO $ do
               nullp <- newForeignPtr_ nullPtr
               (findMinMax' (unS i) (BareImage nullp))

-- | Finds the minimum and maximum value for pixels with non-zero mask value.
findMinMaxMask i mask  = unsafePerformIO (findMinMax' i mask)

-- let a = getAllPixels i in (minimum a,maximum a)

-- | Utility functions for getting the maximum or minimum pixel value of the 
--   image; equal to @snd . findMinMax@ and @fst . findMinMax@.
maxValue,minValue :: Image GrayScale D32 -> D32
maxValue = snd.findMinMax
minValue = fst.findMinMax

-- | Render image of 2D gaussian curve with standard deviation of (stdX,stdY) to image size (w,h)
--   The origin/center of curve is in center of the image.
gaussianImage :: (Int,Int) -> (Double,Double) -> Image GrayScale D32
gaussianImage (w,h) (stdX,stdY) = unsafePerformIO $ do
    dst <- create (w,h) -- 32F_C1
    withImage dst $ \d-> do
                           {#call render_gaussian#} d (realToFrac stdX) (realToFrac stdY)
                           return dst

-- | Produce white image with 'edgeW' amount of edges fading to black.
fadedEdgeImage (w,h) edgeW = unsafePerformIO $ creatingImage ({#call fadedEdges#} w h edgeW)

-- | Produce image where pixel is coloured according to distance from the edge.
fadeToCenter (w,h) = unsafePerformIO $ creatingImage ({#call rectangularDistance#} w h )

-- TODO: Fix C-code of masked_merge to accept D8 input for the mask
-- | Merge two images according to a mask. Result is @R = A*m + B*(m-1)@.
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
--   than given in the distance map that fully covers the previous one.
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
