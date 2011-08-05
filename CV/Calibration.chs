{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
#include "cvWrapLEO.h"
-- | This module is for camera calibration using a chessboard rig.

module CV.Calibration (findChessboardCorners, drawChessboardCorners, defaultFlags, FindFlags(..)) where
{-#OPTIONS-GHC -fwarn-unused-imports #-}
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits

import CV.Image 

import C2HSTools
import Utils.Point
import Control.Applicative
{#import CV.Image#}

#c
enum FindFlags {
     AdaptiveThresh  = CV_CALIB_CB_ADAPTIVE_THRESH
    ,NormalizeImage  = CV_CALIB_CB_NORMALIZE_IMAGE
    ,FilterQuads     = CV_CALIB_CB_FILTER_QUADS
    ,FastCheck       = CV_CALIB_CB_FAST_CHECK
    };
#endc

{#enum FindFlags {}#}

flagsToNum fs = foldl (.|.) 0 $ map (fromIntegral . fromEnum) fs

-- Default flags for finding corners
defaultFlags = [AdaptiveThresh]

-- | Find the inner corners of a chessboard in a given image. 
findChessboardCorners :: CV.Image.Image RGB D8 -> (Int, Int) -> [FindFlags] -> [(Double,Double)]
findChessboardCorners image (w,h) flags =
   unsafePerformIO $ 
    with 1 $ \(c_corner_count::Ptr CInt) -> 
     allocaArray len $ \(c_corners :: Ptr CvPoint )-> 
      withGenImage image $ \c_image -> do
        r <- {#call wrapFindChessBoardCorners#} c_image (fromIntegral w) (fromIntegral h)
                                           (castPtr c_corners) c_corner_count 
                                           (flagsToNum flags)
        count <- peek c_corner_count
        arr <- peekArray (fromIntegral count) c_corners
        return (map cvPt2Pt arr) 
  where len = w*h

-- Draw the found chessboard corners to an image
drawChessboardCorners
  :: CV.Image.Image RGB D8 -> (Int, Int) -> [(Double,Double)] -> CV.Image.Image RGB D8
drawChessboardCorners image (w,h) corners =
   unsafePerformIO $ 
    withClone image $ \clone -> 
     withArray (map pt2CvPt corners) $ \(c_corners :: Ptr CvPoint )-> 
      withGenImage clone$ \c_image -> do
        r <- {#call wrapDrawChessBoardCorners#} c_image (fromIntegral w) (fromIntegral h)
                                           (castPtr c_corners) (fromIntegral $ length corners) 
                                           (found)
        return clone
  where 
    len = w*h
    found | (w*h) == length corners = 1
          | otherwise = 0 
    
newtype CvPoint = CvPt (CFloat,CFloat) deriving (Show)
cvPt2Pt (CvPt (a,b)) = (realToFrac a , realToFrac b)
pt2CvPt (a,b) = CvPt (realToFrac a , realToFrac b)

instance Storable CvPoint where
  sizeOf _ = {#sizeof CvPoint #}
  alignment _ = {#alignof CvPoint2D32f #}
  peek p = CvPt <$> ((,) 
    <$> {#get CvPoint2D32f->x #} p
    <*> {#get CvPoint2D32f->y #} p)
  poke p (CvPt (hx,hy)) = do
    {#set CvPoint2D32f.x #} p (hx)
    {#set CvPoint2D32f.y #} p (hy)

