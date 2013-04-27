{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
-- | This module contains functions for extracting features from connected components
--   of black and white images as well as extracting other shape related features. 
module CV.ConnectedComponents
       (
       -- * Working with connected components
        fillConnectedComponents
       ,maskConnectedComponent
       ,selectSizedComponents
       ,ContourMode(..)
       ,countBlobs
       -- * Working with Image moments
       -- |Note that these functions should probably go to a different module, since
       --  they deal with entire moments of entire images.
       ,spatialMoments
       ,centralMoments
       ,normalizedCentralMoments
       ,huMoments
       -- * Working with component contours aka. object boundaries.
       -- |This part is really old code and probably could be improved a lot.
       ,Contour
       ,drawContour
       ,getContours
       ,contourArea
       ,contourPerimeter
       ,contourPoints
       ,contourHuMoments)
where
#include "cvWrapLEO.h"

import CV.Bindings.ImgProc
import CV.Bindings.Types
import Control.Monad ((>=>))
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.List (foldl')
{#import CV.Image#}

import CV.ImageOp

fillConnectedComponents :: Image GrayScale D8 -> (Image GrayScale D8, Int)
fillConnectedComponents image = unsafePerformIO $ do
  let
    count :: CInt
    count = 0
  withCloneValue image $ \clone ->
    withImage clone $ \pclone ->
      with count $ \pcount -> do
        c'fillConnectedComponents (castPtr pclone) pcount
        c <- peek pcount
        return (clone, fromIntegral c)

maskConnectedComponent :: Image GrayScale D8 -> Int -> Image GrayScale D8
maskConnectedComponent image index = unsafePerformIO $
  withCloneValue image $ \clone ->
    withImage image $ \pimage ->
      withImage clone $ \pclone -> do
        c'maskConnectedComponent (castPtr pimage) (castPtr pclone) (fromIntegral index)
        return clone

-- |Count the number of connected components in the image
countBlobs :: Image GrayScale D8 -> Int 
countBlobs image = fromIntegral $ unsafePerformIO $ do
    withGenImage image $ \i ->
     {#call blobCount#} i

-- |Remove all connected components that fall outside of given size range from the image.
selectSizedComponents :: Double -> Double -> ContourMode -> Image GrayScale D8 -> Image GrayScale D8
selectSizedComponents minSize maxSize mode image = unsafePerformIO $ do
    withGenImage image $ \i ->
     creatingImage ({#call sizeFilter#} i (realToFrac minSize) (realToFrac maxSize) (fromIntegral $ fromEnum mode))

data Level = Immediate | Nested | RecursivelyNested
    deriving (Eq, Ord, Show, Read, Enum)

drawContour :: D8       -- ^ Outer color
            -> D8       -- ^ Hole color
            -> Level    -- ^ Nesting level
            -- -> Thickness
            -- -> LineType
            -> Image GrayScale D8
            -> Contour  -- ^ Contour to draw
            -> Image GrayScale D8
drawContour color holeColor level image contour = unsafePerformIO $ do
    res <- cloneImage image
    withGenImage res $ \i ->
      withContour contour $ \c ->
        {#call draw_contour#} i (castPtr c) (fromIntegral color)
                                  (fromIntegral holeColor)
                                  (fromIntegral $ fromEnum level)
                                  0 -- Thickness
                                  0 -- Line type
    return res

{-# RULES
    "foldl/drawContour"
        forall c hc l. foldl (drawContour c hc l) = drawContours c hc l
  #-}

{-# RULES
    "foldl'/drawContour"
        forall c hc l. foldl' (drawContour c hc l) = drawContours c hc l
  #-}

drawContours :: D8               -- ^ Outer color
             -> D8               -- ^ Hole color
             -> Level            -- ^ Nesting level
             -- -> Thickness
             -- -> LineType
             -> Image GrayScale D8
             -> [Contour]        -- ^ Contour to draw
             -> Image GrayScale D8
drawContours color holeColor level image cs = unsafePerformIO $ do
    res <- cloneImage image
    withGenImage res $ \i -> do
      let ap ctr = do
            withContour ctr $ \cp ->
             {#call draw_contour#} i (castPtr cp) (fromIntegral color)
                                        (fromIntegral holeColor)
                                        (fromIntegral $ fromEnum level)
                                        0 -- Thickness
                                        0 -- Line type
      mapM_ ap cs
    return res

#c
enum ContourMode {
    ContourExternal = CV_RETR_EXTERNAL
    , ContourAll    = CV_RETR_LIST
    , ContourBasicHeirarchy = CV_RETR_CCOMP
    , ContourFullHeirarchy  = CV_RETR_TREE
    };
#endc

{#enum ContourMode {} #}

-- * Working with Image moments. 

-- Utility function for getting the moments
getMoments :: (Ptr C'CvMoments -> CInt -> CInt -> IO (CDouble)) -> Image GrayScale D32 -> Bool -> [Double]
getMoments f image binary = unsafePerformIO $ do
  withImage image $ \pimage -> do
    let
      moments :: C'CvMoments
      moments = C'CvMoments 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    with moments $ \pmoments -> do
      c'cvMoments (castPtr pimage) pmoments (if binary then 1 else 0)
      ms <- sequence [ f pmoments i j
                       | i <- [0..3], j <- [0..3], i+j <= 3 ]
      return (map realToFrac ms)

-- | Extract raw spatial moments of the image.
spatialMoments = getMoments c'cvGetSpatialMoment

-- | Extract central moments of the image. These are useful for describing the
--   object shape for a classifier system.
centralMoments = getMoments c'cvGetCentralMoment

-- | Extract normalized central moments of the image.
normalizedCentralMoments = getMoments c'cvGetNormalizedCentralMoment

{-
centralMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   ms <- sequence [{#call cvGetCentralMoment#} moments i j
                  | i <- [0..3], j<-[0..3], i+j <= 3]
   {#call freeCvMoments#} moments
   return (map realToFrac ms)
-}

-- |Extract Hu-moments of the image. These features are rotation invariant.
huMoments :: Image GrayScale D32 -> Bool -> [Double]
huMoments image binary = unsafePerformIO $ do
  withImage image $ \pimage -> do
    let
      moments = C'CvMoments 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      hu = C'CvHuMoments 0 0 0 0 0 0 0
    with moments $ \pmoments -> do
      with hu $ \phu -> do
        c'cvMoments (castPtr pimage) pmoments (if binary then 1 else 0)
        c'cvGetHuMoments pmoments phu
        (C'CvHuMoments hu1 hu2 hu3 hu4 hu5 hu6 hu7) <- peek phu
        return (map realToFrac [hu1,hu2,hu3,hu4,hu5,hu6,hu7])

{-
huMoments image binary = unsafePerformIO $ do
   moments <- withImage image $ \i -> {#call getMoments#} i (if binary then 1 else 0)
   hu <- readHu moments
   {#call freeCvMoments#} moments
   return (map realToFrac hu)
-}

-- read stuff out of hu-moments structure.. This could be done way better.
readHu m = do
   hu <- mallocArray 7
   {#call getHuMoments#} m hu
   hu' <- peekArray 7 hu
   free hu
   return hu'

-- |Structure that contains the opencv sequence holding the contour data.
{#pointer *FoundContour as Contour foreign newtype#}
foreign import ccall "& free_found_contour" releaseContour
    :: FinalizerPtr Contour

-- |Extract contours of connected components of the image.
getContours :: Image GrayScale D8 -> [Contour]
getContours img = unsafePerformIO $ do
        withImage img $ \i -> do
          ptrCS <- {#call get_contours#} i
          let loop = do
                ptrNew <- {#call get_contour#} ptrCS
                if nullPtr == ptrNew
                    then return []
                    else do cptr <- newForeignPtr releaseContour ptrNew
                            (Contour cptr :) `fmap` loop
          loop

wContour :: (Ptr a -> IO b) -> Contour -> IO b
wContour o c = withContour c (\p -> o (castPtr p))

printContour :: Contour -> IO ()
printContour = wContour {#call print_contour#}

contourArea :: Contour -> Double
contourArea = unsafePerformIO . wContour ({#call contour_area#} >=> return.realToFrac)
-- ^The area of a contour.

contourPerimeter :: Contour -> Double
contourPerimeter = unsafePerformIO . wContour ({#call contour_perimeter#} >=> return.realToFrac)
-- ^Get the perimeter of a contour.

-- |Get a list of the points in the contour.
contourPoints :: Contour -> [(Double,Double)]
contourPoints c = unsafePerformIO $ withContour c $ \ptr -> do
     count <- {#call cur_contour_size#} (castPtr ptr)
     let count' = fromIntegral count
     ----print count
     xs <- mallocArray count'
     ys <- mallocArray count'
     {#call contour_points#} (castPtr ptr) xs ys
     xs' <- peekArray count' xs
     ys' <- peekArray count' ys
     free xs
     free ys
     return $ zip (map fromIntegral xs') (map fromIntegral ys')

-- | Operation for extracting Hu-moments from a contour
contourHuMoments :: Contour -> [Double]
contourHuMoments = unsafePerformIO . (getContourHuMoments' >=> return.map realToFrac)
getContourHuMoments' = wContour (\c -> do
   m <- {#call contour_moments#} c
   hu <- readHu m
   {#call freeCvMoments#} m
   return hu)


{#pointer *CvMoments as Moments foreign newtype#}
