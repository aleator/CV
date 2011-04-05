{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.ConnectedComponents
--    (selectSizedComponents,countBlobs,centralMoments
--    ,huMoments,Contours,getContours) 
where

import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe
import Foreign.ForeignPtr

import C2HSTools

{#import CV.Image#}

import CV.ImageOp

countBlobs :: Image GrayScale D8 -> Int 
countBlobs image = fromIntegral $ unsafePerformIO $ do
    withGenImage image $ \i ->
     {#call blobCount#} i

selectSizedComponents minSize maxSize image = unsafePerformIO $ do
    withGenImage image $ \i ->
     creatingImage ({#call sizeFilter#} i minSize maxSize)


{#pointer *CvMoments as Moments foreign newtype#}

-- foreign import ccall "& freeCvMoments" releaseMoments :: FinalizerPtr Moments
   
centralMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   ms <- sequence [{#call cvGetCentralMoment#} moments i j
                  | i <- [0..3], j<-[0..3], i+j <= 3]
   {#call freeCvMoments#} moments
   return ms

huMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   hu <- readHu moments
   {#call freeCvMoments#} moments
   return hu

readHu m = do
   hu <- mallocArray 7
   {#call getHuMoments#} m hu
   hu' <- peekArray 7 hu
   free hu
   return hu'

-- Contours
{#pointer *FoundContours as Contours foreign newtype#}
foreign import ccall "& free_found_contours" releaseContours 
    :: FinalizerPtr Contours

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
contourArea = rawContourOpUS ({#call contour_area#})
contourPerimeter = rawContourOpUS {#call contour_perimeter#}

getContourPoints = rawContourOpUS getContourPoints'
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

getContourHuMoments = rawContourOpUS getContourHuMoments' 
getContourHuMoments' f = do
   m <- {#call contour_moments#} f     
   hu <- readHu m 
   {#call freeCvMoments#} m
   return hu

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
