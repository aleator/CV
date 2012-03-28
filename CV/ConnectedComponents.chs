{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
-- | This module contains functions for extracting features from connected components
--   of black and white images as well as extracting other shape related features. 
module CV.ConnectedComponents
       (
       -- * Working with connected components
        selectSizedComponents
       ,countBlobs
       -- * Working with Image moments
       -- |Note that these functions should probably go to a different module, since
       --  they deal with entire moments of entire images.
       ,centralMoments
       ,huMoments
       -- * Working with component contours aka. object boundaries.
       -- |This part is really old code and probably could be improved a lot.
       ,Contours
       ,getContours
       ,contourArea
       ,contourPerimeter
       ,contourPoints
       ,mapContours
       ,contourHuMoments) 
where
#include "cvWrapLEO.h"

import Control.Monad ((>=>))
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Unsafe

{#import CV.Image#}

import CV.ImageOp



-- |Count the number of connected components in the image
countBlobs :: Image GrayScale D8 -> Int 
countBlobs image = fromIntegral $ unsafePerformIO $ do
    withGenImage image $ \i ->
     {#call blobCount#} i

-- |Remove all connected components that fall outside of given size range from the image.
selectSizedComponents :: Double -> Double -> Image GrayScale D8 -> Image GrayScale D8
selectSizedComponents minSize maxSize image = unsafePerformIO $ do
    withGenImage image $ \i ->
     creatingImage ({#call sizeFilter#} i (realToFrac minSize) (realToFrac maxSize))

-- * Working with Image moments. 


-- |Extract central moments of the image. These are useful for describing the object shape
--  for a classifier system.
centralMoments :: Image GrayScale D32 -> Bool -> [Double]
centralMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   ms <- sequence [{#call cvGetCentralMoment#} moments i j
                  | i <- [0..3], j<-[0..3], i+j <= 3]
   {#call freeCvMoments#} moments
   return (map realToFrac ms)

-- |Extract Hu-moments of the image. These features are rotation invariant.
huMoments :: Image GrayScale D32 -> Bool -> [Double]
huMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   hu <- readHu moments
   {#call freeCvMoments#} moments
   return (map realToFrac hu)

-- read stuff out of hu-moments structure.. This could be done way better.
readHu m = do
   hu <- mallocArray 7
   {#call getHuMoments#} m hu
   hu' <- peekArray 7 hu
   free hu
   return hu'

-- |Structure that contains the opencv sequence holding the contour data.
{#pointer *FoundContours as Contours foreign newtype#}
foreign import ccall "& free_found_contours" releaseContours 
    :: FinalizerPtr Contours

-- | This function maps an opencv contour calculation over all
--   contours of the image. 
mapContours :: ContourFunctionUS a -> Contours -> [a]
mapContours (CFUS op) contours = unsafePerformIO $ do
    let loop acc cp = do
        more <- withContours cp {#call more_contours#}
        if more < 1 
            then return acc 
            else do
                x <- op cp
                (i::CInt) <- withContours cp {#call next_contour#}
                loop (x:acc) cp
         
    acc <- loop [] contours
    withContours contours ({#call reset_contour#})
    return acc

-- |Extract contours of connected components of the image.
getContours :: Image GrayScale D8 -> Contours
getContours img = unsafePerformIO $ do
        withImage img $ \i -> do
          ptr <- {#call get_contours#} i
          fptr <- newForeignPtr releaseContours ptr
          return $ Contours fptr 

newtype ContourFunctionUS a = CFUS (Contours -> IO a)
newtype ContourFunctionIO a = CFIO (Contours -> IO a)

rawContourOpUS op = CFUS $ \c -> withContours c op
rawContourOp op = CFIO $ \c -> withContours c op

printContour = rawContourOp {#call print_contour#}

contourArea :: ContourFunctionUS Double
contourArea = rawContourOpUS ({#call contour_area#} >=> return.realToFrac)
-- ^The area of a contour.

contourPerimeter :: ContourFunctionUS Double
contourPerimeter = rawContourOpUS $ {#call contour_perimeter#} >=> return.realToFrac
-- ^Get the perimeter of a contour.

-- |Get a list of the points in the contour.
contourPoints :: ContourFunctionUS [(Double,Double)]
contourPoints = rawContourOpUS getContourPoints'
getContourPoints' f = do
     count <- {#call cur_contour_size#} f
     let count' = fromIntegral count 
     ----print count
     xs <- mallocArray count'     
     ys <- mallocArray count'
     {#call contour_points#} f xs ys
     xs' <- peekArray count' xs
     ys' <- peekArray count' ys
     free xs
     free ys
     return $ zip (map fromIntegral xs') (map fromIntegral ys')

-- | Operation for extracting Hu-moments from a contour
contourHuMoments :: ContourFunctionUS [Double]
contourHuMoments = rawContourOpUS $ getContourHuMoments' >=> return.map realToFrac
getContourHuMoments' f = do
   m <- {#call contour_moments#} f     
   hu <- readHu m 
   {#call freeCvMoments#} m
   return hu


mapContoursIO :: ContourFunctionIO a -> Contours -> IO [a]
mapContoursIO (CFIO op) contours = do
    let loop acc cp = do
        more <- withContours cp {#call more_contours#}
        if more < 1 
            then return acc 
            else do
                x <- op cp
                (i::CInt) <- withContours cp {#call next_contour#}
                loop (x:acc) cp
         
    acc <- loop [] contours
    withContours contours ({#call reset_contour#})
    return acc

{#pointer *CvMoments as Moments foreign newtype#}
