{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
#include "cvWrapLEO.h"
-- | This module is for camera calibration using a chessboard rig.

module CV.Calibration (findChessboardCorners,refineChessboardCorners, drawChessboardCorners, defaultFlags, FindFlags(..), calibrateCamera2) where
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

import CV.Matrix
import CV.Bindings.Calibrate

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
findChessboardCorners :: CV.Image.Image RGB D8 -> (Int, Int) -> [FindFlags] -> [(Float,Float)]
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

refineChessboardCorners :: Image GrayScale D8 -> [(Float,Float)] -> (Int,Int) -> (Int,Int) 
                                        -> [(Float,Float)]
refineChessboardCorners img pts (winW,winH) (zeroW,zeroH) = unsafePerformIO $ do
    with 1 $ \(c_corner_count::Ptr CInt) -> 
      withImage img $ \c_img ->
      withArray (map mkPt pts) $ \(c_corners :: Ptr C'CvPoint2D32f ) -> do 
        c'wrapFindCornerSubPix c_img c_corners (length pts) winW winH zeroW zeroH tType maxIter epsilon 
        map fromPt `fmap` peekArray (length pts) c_corners
 where
    mkPt (x,y) = C'CvPoint2D32f x y
    fromPt (C'CvPoint2D32f x y) = (x,y)
    tType = c'CV_TERMCRIT_ITER
    maxIter = 100
    epsilon = 0.01

    

-- Draw the found chessboard corners to an image
drawChessboardCorners
  :: CV.Image.Image RGB D8 -> (Int, Int) -> [(Float,Float)] -> CV.Image.Image RGB D8
drawChessboardCorners image (w,h) corners =
   unsafePerformIO $ 
    withCloneValue image $ \clone -> 
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

--calibrateCamera2 :: [[((Float,Float,Float),(Float,Float))]] -> (Int,Int) -> IO Double
calibrateCamera2 views (w,h) = do
    let 
        pointCounts :: Matrix Int
        pointCounts  = fromList (1,length views) (map (length) views)
        m = length views
        totalPts = length (concat views)
        objectPoints :: Matrix Float
        objectPoints = fromList (3,totalPts) $ concat [[x,y,z] | ((x,y,z),_) <- concat views]
        imagePoints :: Matrix Float
        imagePoints  = fromList (2,totalPts) $ concat [[x,y]   | (_,(x,y))   <- concat views]
        flags = c'CV_CALIB_FIX_K1
                .|.  c'CV_CALIB_FIX_K1
                .|.  c'CV_CALIB_FIX_K2
                .|.  c'CV_CALIB_FIX_K3
                .|.  c'CV_CALIB_FIX_K4
                .|.  c'CV_CALIB_FIX_K5
                .|.  c'CV_CALIB_FIX_K6
                .|.  c'CV_CALIB_ZERO_TANGENT_DIST

        size = C'CvSize (fromIntegral w) (fromIntegral h)
        cameraMatrix,distCoeffs,rvecs,tvecs :: Matrix Float
        cameraMatrix = emptyMatrix (3,3)
        distCoeffs   = emptyMatrix (1,8)
        rvecs        = emptyMatrix (m,3)
        tvecs        = emptyMatrix (m,3)

    err <- with size $ \c_size ->
     withMatPtr objectPoints $ \c_objectPoints ->
     withMatPtr imagePoints $ \c_imagePoints ->
     withMatPtr pointCounts $ \c_pointCounts ->
     withMatPtr cameraMatrix $ \c_cameraMatrix ->
     withMatPtr distCoeffs $ \c_distCoeffs ->
     withMatPtr rvecs $ \c_rvecs ->
     withMatPtr tvecs $ \c_tvecs ->
      c'wrapCalibrateCamera2 c_objectPoints c_imagePoints c_pointCounts c_size 
                             c_cameraMatrix c_distCoeffs c_rvecs c_tvecs flags

    -- print ( objectPoints, imagePoints, pointCounts,cameraMatrix, distCoeffs, rvecs, tvecs )
    return (err, transpose cameraMatrix, toCols distCoeffs, toCols rvecs, toCols tvecs)

