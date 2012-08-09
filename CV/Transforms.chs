{-#LANGUAGE ForeignFunctionInterface, ViewPatterns, ScopedTypeVariables, PatternGuards, FlexibleContexts#-}
#include "cvWrapLEO.h"
-- |Various image transformations from opencv and other sources.
module CV.Transforms  where

import CV.Image as I 
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import System.IO.Unsafe
{#import CV.Image#}
import CV.ImageMathOp
import qualified CV.Matrix as M
import CV.Matrix (Matrix,withMatPtr)

-- |Since DCT is valid only for even sized images, we provide a
-- function to crop images to even sizes.
takeEvenSized img = getRegion (0,0) (w-wadjust,h-hadjust) img
    where
     (w,h) = getSize img
     hadjust | odd h = 1
             | otherwise = 2
     wadjust | odd w = 1
             | otherwise = 2

-- |Perform Discrete Cosine Transform
dct :: Image GrayScale d -> Image GrayScale d
dct img | (x,y) <- getSize img, even x && even y 
        = unsafePerformIO $
            withGenImage img $ \i -> 
              withClone img $ \c' -> 
                withGenImage c' $ \c ->
                  ({#call cvDCT#} i c 0)
        | otherwise = error "DCT needs even sized image"

-- |Perform Inverse Discrete Cosine Transform
idct :: Image GrayScale d -> Image GrayScale d
idct img | (x,y) <- getSize img, even x && even y 
        = unsafePerformIO $
            withGenImage img $ \i -> 
              withClone img $ \c' -> 
                withGenImage c' $ \c ->
                  ({#call cvDCT#} i c 1)
        | otherwise = error "IDCT needs even sized image"

data MirrorAxis = Vertical | Horizontal deriving (Show,Eq)

-- |Mirror an image over a cardinal axis
flip :: CreateImage (Image c d) => MirrorAxis -> Image c d -> Image c d
flip axis img = unsafePerformIO $ do
                 cl <- I.create (getSize img)
                 withGenImage img $ \cimg -> 
                  withGenImage cl $ \ccl -> do
                    {#call cvFlip#} cimg ccl (if axis == Vertical then 0 else 1)
                 return cl

-- |Rotate `img` `angle` radians.
rotate :: Double -> Image c d -> Image c d
rotate (realToFrac -> angle) img = unsafePerformIO $
                    withImage img $ \i -> 
                        creatingImage 
                         ({#call rotateImage#} i 1 angle)

data Interpolation = NearestNeighbour | Linear
                   | Area | Cubic
                deriving (Eq,Ord,Enum,Show)

-- |Simulate a radial distortion over an image
radialDistort :: Image GrayScale D32 -> Double -> Image GrayScale D32
radialDistort img k = unsafePerformIO $ do
                       target <- I.create (getSize img)
                       withImage img $ \cimg ->
                        withImage target $ \ctarget ->
                         {#call radialRemap#} cimg ctarget (realToFrac k)
                       return target

-- |Scale image by one ratio on both of the axes
scaleSingleRatio tpe x img = scale tpe (x,x) img


-- |Scale an image with different ratios for axes
scale :: (CreateImage (Image c D32), RealFloat a) => Interpolation -> (a,a) -> Image c D32 -> Image c D32
scale tpe (x,y) img = unsafePerformIO $ do
                    target <- I.create (w',h') 
                    withGenImage img $ \i -> 
                     withGenImage target $ \t -> 
                        {#call cvResize#} i t 
                            (fromIntegral.fromEnum $ tpe)
                    return target
            where
             (w,h) = getSize img
             (w',h') = (round $ fromIntegral w*y
                       ,round $ fromIntegral h*x)

-- |Scale an image to a given size
scaleToSize :: (CreateImage (Image c D32)) => 
    Interpolation -> Bool -> (Int,Int) -> Image c D32 -> Image c D32
scaleToSize tpe retainRatio (w,h) img = unsafePerformIO $ do
                    target <- I.create (w',h') 
                    withGenImage img $ \i -> 
                     withGenImage target $ \t -> 
                        {#call cvResize#} i t 
                            (fromIntegral.fromEnum $ tpe)
                    return target
            where
             (ow,oh) = getSize img
             (w',h') = if retainRatio 
                         then (floor $ fromIntegral ow*ratio,floor $ fromIntegral oh*ratio)
                         else (w,h)
             ratio  = max (fromIntegral w/fromIntegral ow)
                          (fromIntegral h/fromIntegral oh)

-- |Apply a perspective transform to the image. The transformation 3x3 matrix is supplied as
--  a row ordered, flat, list.
perspectiveTransform :: Real a => Image c d -> [a] -> Image c d
perspectiveTransform img (map realToFrac -> [a1,a2,a3,a4,a5,a6,a7,a8,a9])
    = unsafePerformIO $ 
       withImage img $ \cimg -> creatingImage $ {#call wrapPerspective#} cimg a1 a2 a3 a4 a5 a6 a7 a8 a9

perspectiveTransform' :: (CreateImage (Image c d)) => Matrix Float -> Image c d -> (Int,Int)-> Image c d
perspectiveTransform' mat img size
    = unsafePerformIO $ do
       r <- create  size
       withImage img $ \c_img ->
         withMatPtr mat $ \c_mat ->
         withImage r $ \c_r -> {#call wrapWarpPerspective#} (castPtr c_img) (castPtr c_r) (castPtr c_mat)
       return r


-- |Find a homography between two sets of points in. The resulting 3x3 matrix is returned as a list.
getHomography srcPts dstPts = 
    unsafePerformIO $ withArray src $ \c_src ->
                       withArray dst $ \c_dst ->
                        allocaArray (3*3) $ \c_hmg -> do
                         {#call findHomography#} c_src c_dst (fromIntegral $ length srcPts) c_hmg
                         peekArray (3*3) c_hmg
    where
     flatten = map realToFrac . concatMap (\(a,b) -> [a,b]) 
     src = flatten srcPts
     dst = flatten dstPts

#c
enum HomographyMethod {
     Default = 0,
     Ransac  = CV_RANSAC,
     LMeds   = CV_LMEDS
     };
#endc
{#enum HomographyMethod {}#}

getHomography' :: Matrix Float -> Matrix Float -> HomographyMethod -> Float -> Matrix Float
getHomography' srcPts dstPts method ransacThreshold = 
    unsafePerformIO $ do
    hmg <- M.create (3,3) :: IO (Matrix Float)
    withMatPtr srcPts  $ \c_src ->
     withMatPtr dstPts $ \c_dst ->
     withMatPtr hmg    $ \c_hmg -> do
                          {#call cvFindHomography#} 
                           (castPtr c_src) 
                           (castPtr c_dst) 
                           (castPtr c_hmg)
                           (fromIntegral $ fromEnum method)
                           (realToFrac ransacThreshold)
                           nullPtr
                          return hmg

--- Pyramid transforms
-- |Return a copy of an image with an even size
evenize :: Image channels depth -> Image channels depth
evenize img = if (odd w || odd h)
              then  
                unsafePerformIO $     
                 creatingImage $
                  withGenImage img $ \cImg -> {#call makeEvenUp#} cImg
              else img
    where
     (w,h)  = getSize img

-- |Return a copy of an image with an odd size
oddize :: Image channels depth -> Image channels depth
oddize img = if (even w || even h)
              then  
                unsafePerformIO $     
                 creatingImage $
                  withGenImage img $ \cImg -> {#call padUp#} cImg (toI $ even w) (toI $ even h)
              else img
    where
     toI True = 1
     toI False = 0
     (w,h)  = getSize img

-- |Pad images to same size
sameSizePad :: Image channels depth -> Image c d -> Image channels depth
sameSizePad img img2 = if (size1 /= size2)
              then unsafePerformIO $ do
                r <- creatingImage $
                       withGenImage img2 $ \cImg -> {#call padUp#} cImg (toI $ w2<w1) (toI $ h2<h1)
                if getSize r /= getSize img 
                    then error ("Couldn't pad: "++show size1++"/"++show size2) 
                    else return r
              else img
    where
     toI True = 1
     toI False = 0
     size1@(w1,h1)  = getSize img
     size2@(w2,h2)  = getSize img2



cv_Gaussian = 7
-- |Downsize image by 50% efficiently. Image dimensions must be even.
pyrDown ::(CreateImage (Image GrayScale a)) => Image GrayScale a -> Image GrayScale a
pyrDown image = unsafePerformIO $ do
                 res <- I.create size 
                 withGenImage image $ \cImg -> 
                   withGenImage res $ \cResImg -> 
                     {#call cvPyrDown#} cImg cResImg cv_Gaussian
                 return res
            where
                size = (x`div`2,y`div`2)
                (x,y) = getSize image  

-- |Enlarge image to double in each dimension. Used to recover pyramidal layers
pyrUp :: (CreateImage (Image GrayScale a)) => Image GrayScale a -> Image GrayScale a
pyrUp image = unsafePerformIO $ do
                 res <- I.create size 
                 withGenImage image $ \cImg -> 
                   withGenImage res $ \cResImg -> 
                     {#call cvPyrUp#} cImg cResImg cv_Gaussian
                 return res
            where
                size = (x*2,y*2)
                (x,y) = getSize image  


-- TODO: For additional efficiency, make this so that pyrDown result is directly put into
--       proper size image which is then padded
safePyrDown img = evenize result
    where
     result = pyrDown img 
     (w,h)  = getSize result 

-- | Enlargen the image so that its size is a power of two.
minEnlarge :: Image GrayScale D32 -> Image GrayScale D32
minEnlarge i = enlargeShadow (min (ceiling (logBase 2 (f w))) (ceiling (logBase 2 (f h)))) i
    where 
     f = fromIntegral
     (w,h) = getSize i

-- | Calculate an infinite gaussian pyramid of an image while keeping track of
--   various corner cases and gotchas.
gaussianPyramid :: Image GrayScale D32 -> [Image GrayScale D32]
gaussianPyramid = iterate pyrDown' . minEnlarge
    where 
     pyrDown' i = let (w,h) = getSize i
                  in if (w`div`2) <=1 || (h`div`2) <= 1 then i else pyrDown i

-- |Calculate the laplacian pyramid of an image up to the nth level.
--  Notice that the image size must be divisible by 2^n or opencv 
--  will abort (TODO!)
laplacianPyramid :: Int -> Image GrayScale D32 -> [Image GrayScale D32]
laplacianPyramid depth image = reverse laplacian
  where
   downs :: [Image GrayScale D32] = take depth $ iterate pyrDown (image)
   upsampled :: [Image GrayScale D32] = map pyrUp (tail downs)
   laplacian = zipWith (#-) downs upsampled ++ [last downs]

-- |Reconstruct an image from a laplacian pyramid
reconstructFromLaplacian :: [Image GrayScale D32] -> Image GrayScale D32 
reconstructFromLaplacian pyramid = foldl1 (\a b -> (pyrUp a) #+ b) (pyramid)
  --  where 
  --   safeAdd x y = sameSizePad y x #+ y  

-- TODO: Could have wider type
-- |Enlargen the image so that its size is divisible by 2^n. Fill the area
--  outside the image with black.
enlarge :: Int -> Image GrayScale D32 -> Image GrayScale D32
enlarge n img =  unsafePerformIO $ do
                   i <- I.create (w2,h2)
                   blit i img (0,0)
                   return i
    where
     (w,h) = getSize img
     (w2,h2) = (pad w, pad h)
     pad x = x + (np - x `mod` np)
     np = 2^n

-- | Enlargen the image so that its size is is divisible by 2^n. Replicate
--   the border of the image.
enlargeShadow :: Int -> Image GrayScale D32 -> Image GrayScale D32
enlargeShadow n img =  unsafePerformIO $ do
                   i <- create (w2,h2)
                   withImage img $ \c_img -> 
                    withImage i  $ \c_i   -> {#call blitShadow#} c_i c_img 
                   return i
    where
     (w,h) = getSize img
     (w2,h2) = (pad w, pad h)
     pad x = x + (np - x `mod` np)
     np = 2^n

#c
enum DistanceType {
     C =  CV_DIST_C
    ,L1 =  CV_DIST_L1
    ,L2 =  CV_DIST_L2
};
#endc
{#enum DistanceType {}#}
#ifdef OpenCV24
#c
enum LabelType {
     CCOMP = CV_DIST_LABEL_CCOMP
    ,PIXEL = CV_DIST_LABEL_PIXEL
};
#endc
{#enum LabelType {}#}
#endif

-- |Mask sizes accepted by distanceTransform
data MaskSize = M3 | M5 deriving (Eq,Ord,Enum,Show)

-- |Perform a distance transform on the image
distanceTransform :: DistanceType -> MaskSize -> Image GrayScale D8 -> Image GrayScale D32 --TODO: Input should be a black and white image
distanceTransform dtype maskSize source = unsafePerformIO $ do
    result :: Image GrayScale D32 <- I.create (getSize source)
    withGenImage source $ \c_source ->
     withGenImage result $ \c_result ->
        {#call cvDistTransform #} c_source c_result 
                                  (fromIntegral . fromEnum $ dtype) 
                                  (fromIntegral . fromEnum $ maskSize)
                                   nullPtr nullPtr
#ifdef OpenCV24
                                  (fromIntegral . fromEnum $ CCOMP)
#endif

    return result

    -- TODO: Add handling for labels
    -- TODO: Add handling for custom masks
