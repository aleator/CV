{-#LANGUAGE ForeignFunctionInterface, ViewPatterns, ScopedTypeVariables, PatternGuards, FlexibleContexts#-}
#include "cvWrapLEO.h"
module CV.Transforms where

import CV.Image
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import System.IO.Unsafe
{#import CV.Image#}
import CV.ImageMathOp

-- Since DCT is valid only for even sized images, we provide a
-- function to crop images to even sizes.
takeEvenSized img = getRegion (0,0) (w-wadjust,h-hadjust) img
    where
     (w,h) = getSize img
     hadjust | odd h = 1
             | otherwise = 2
     wadjust | odd w = 1
             | otherwise = 2

-- Perform Discrete Cosine Transform
dct img | (x,y) <- getSize img, even x && even y 
        = unsafePerformIO $
            withGenImage img $ \i -> 
              withClone img $ \c' -> 
                withGenImage c' $ \c ->
                  ({#call cvDCT#} i c 0)
        | otherwise = error "DCT needs even sized image"

idct img | (x,y) <- getSize img, even x && even y 
        = unsafePerformIO $
            withGenImage img $ \i -> 
              withClone img $ \c' -> 
                withGenImage c' $ \c ->
                  ({#call cvDCT#} i c 1)
        | otherwise = error "IDCT needs even sized image"

data MirrorAxis = Vertical | Horizontal deriving (Show,Eq)

flip axis img = unsafePerformIO $ do
                 cl <- emptyCopy img
                 withGenImage img $ \cimg -> 
                  withGenImage cl $ \ccl -> do
                    {#call cvFlip#} cimg ccl (if axis == Vertical then 0 else 1)
                 return cl

-- Rotate `img` `angle` radians.
rotate angle img = unsafePerformIO $
                    withImage img $ \i -> 
                        creatingImage 
                         ({#call rotateImage#} i 1 angle)

data Interpolation = NearestNeighbour | Linear
                   | Area | Cubic
                deriving (Eq,Ord,Enum,Show)

radialDistort :: Image GrayScale D32 -> Double -> Image GrayScale D32
radialDistort img k = unsafePerformIO $ do
                       target <- emptyCopy img 
                       withImage img $ \cimg ->
                        withImage target $ \ctarget ->
                         {#call radialRemap#} cimg ctarget (realToFrac k)
                       return target

scale :: (RealFloat a) => Interpolation -> a -> Image GrayScale D32 -> Image GrayScale D32
scale tpe size img = unsafePerformIO $ do
                    target <- create (w',h') 
                    withGenImage img $ \i -> 
                     withGenImage target $ \t -> 
                        {#call cvResize#} i t 
                            (fromIntegral.fromEnum $ tpe)
                    return target
            where
             (w,h) = getSize img
             (w',h') = (round $ fromIntegral w*size
                       ,round $ fromIntegral h*size)

scaleToSize :: Interpolation -> Bool -> (Int,Int) -> Image GrayScale D32 -> Image GrayScale D32
scaleToSize tpe retainRatio (w,h) img = unsafePerformIO $ do
                    target <- create (w',h') 
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

oneParamPerspective img k
    = unsafePerformIO $ 
       withImage img $ \cimg -> creatingImage $ {#call simplePerspective#} k cimg

perspectiveTransform img (map realToFrac -> [a1,a2,a3,a4,a5,a6,a7,a8,a9])
    = unsafePerformIO $ 
       withImage img $ \cimg -> creatingImage $ {#call wrapPerspective#} cimg a1 a2 a3 a4 a5 a6 a7 a8 a9


getHomography srcPts dstPts = 
    unsafePerformIO $ withArray src $ \c_src ->
                       withArray dst $ \c_dst ->
                        allocaArray (3*3) $ \c_hmg -> do
                         {#call findHomography#} c_src c_dst (fromIntegral $ length srcPts) c_hmg
                         peekArray (3*3) c_hmg
    where
     flatten = concatMap (\(a,b) -> [a,b]) 
     src = flatten srcPts
     dst = flatten dstPts


--- Pyramid transforms
evenize img = if (odd w || odd h)
              then  
                unsafePerformIO $     
                 creatingImage $
                  withGenImage img $ \cImg -> {#call makeEvenUp#} cImg
              else img
    where
     (w,h)  = getSize img

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
pyrDown ::(CreateImage (Image GrayScale a)) => Image GrayScale a -> Image GrayScale a
pyrDown image = unsafePerformIO $ do
                 res <- create size 
                 withGenImage image $ \cImg -> 
                   withGenImage res $ \cResImg -> 
                     {#call cvPyrDown#} cImg cResImg cv_Gaussian
                 return res
            where
                size = (x`div`2,y`div`2)
                (x,y) = getSize image  

pyrUp :: (CreateImage (Image GrayScale a)) => Image GrayScale a -> Image GrayScale a
pyrUp image = unsafePerformIO $ do
                 res <- create size 
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

laplacianPyramid :: Int -> Image GrayScale D32 -> [Image GrayScale D32]
laplacianPyramid depth image = reverse laplacian
  where
   downs :: [Image GrayScale D32] = take depth $ iterate pyrDown (image)
   upsampled :: [Image GrayScale D32] = map pyrUp (tail downs)
   laplacian = zipWith (#-) downs upsampled ++ [last downs]

-- | Reconstruct an image from a laplacian pyramid
reconstructFromLaplacian pyramid = foldl1 (\a b -> (pyrUp a) #+ b) (pyramid)
  --  where 
  --   safeAdd x y = sameSizePad y x #+ y  

-- | Enlarge image so, that it's size is divisible by 2^n 
-- TODO: Could have wider type
enlarge :: Int -> Image GrayScale D32 -> Image GrayScale D32
enlarge n img =  unsafePerformIO $ do
                   i <- create (w2,h2)
                   blit i img (0,0)
                   return i
    where
     (w,h) = getSize img
     (w2,h2) = (pad w, pad h)
     pad x = x + (np - x `mod` np)
     np = 2^n


