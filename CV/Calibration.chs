{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
#include "cvWrapLEO.h"
-- | This module exposes opencv functions for camera calibration using a chessboard rig. This module follows opencv quite closely and the best documentation
--   is probably found there. As quick example however, the following program detects and draws chessboard corners from an image.
--
-- @
-- module Main where
-- import CV.Image
-- import CV.Calibration
-- 
-- main = do
--     Just i <- loadColorImage \"chess.png\"
--     let corners = findChessboardCorners (unsafeImageTo8Bit i) (4,5) (FastCheck:defaultFlags)
--     let y = drawChessboardCorners (unsafeImageTo8Bit i) (4,5) corners
--     mapM_ print (corners)
--     saveImage \"found_chessboard.png\" y
-- @
module CV.Calibration 
    (
     -- * Finding chessboard calibration rig
     FindFlags(..)
    ,defaultFlags
    ,findChessboardCorners
    ,refineChessboardCorners
    -- * Visualization
    ,drawChessboardCorners
    -- * Camera calibration
    ,calibrateCamera2
    -- * Rectification
    ,stereoRectifyUncalibrated
    ,findFundamentalMat
    ,c'CV_FM_7POINT
    ,c'CV_FM_8POINT
    ,c'CV_FM_RANSAC
    ,c'CV_FM_LMEDS
    ) where
{-#OPTIONS-GHC -fwarn-unused-imports #-}

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Bits

import CV.Image 

import System.IO.Unsafe
import Utils.Point
import Control.Applicative

import CV.Matrix
import CV.Bindings.Calibrate
import CV.Bindings.Types

{#import CV.Image#}

#c
enum FindFlags {
     AdaptiveThresh  = CV_CALIB_CB_ADAPTIVE_THRESH
    ,NormalizeImage  = CV_CALIB_CB_NORMALIZE_IMAGE
    ,FilterQuads     = CV_CALIB_CB_FILTER_QUADS
    ,FastCheck       = CV_CALIB_CB_FAST_CHECK
    };
#endc

-- | Flags for the chessboard corner detector. See opencv documentation for cvFindChessboardCorners.
{#enum FindFlags {}#}

flagsToNum fs = foldl (.|.) 0 $ map (fromIntegral . fromEnum) fs

-- |Default flags for finding corners
defaultFlags :: [FindFlags]
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

-- |Given an estimate of chessboard corners, provide a subpixel estimation of actual corners.
refineChessboardCorners :: Image GrayScale D8 -> [(Float,Float)] -> (Int,Int) -> (Int,Int) -> [(Float,Float)]
refineChessboardCorners img pts (winW,winH) (zeroW,zeroH) = unsafePerformIO $ do
    with 1 $ \(c_corner_count::Ptr CInt) -> 
      withImage img $ \c_img ->
      withArray (map mkPt pts) $ \(c_corners :: Ptr C'CvPoint2D32f ) -> do 
        c'wrapFindCornerSubPix c_img c_corners (length pts) winW winH zeroW zeroH tType maxIter epsilon 
        map fromPt `fmap` peekArray (length pts) c_corners
 where
    mkPt (x,y) = C'CvPoint2D32f (realToFrac x) (realToFrac y)
    fromPt (C'CvPoint2D32f x y) = (realToFrac x,realToFrac y)
    tType = c'CV_TERMCRIT_ITER
    maxIter = 100
    epsilon = 0.01

-- | Draw the found chessboard corners to an image
drawChessboardCorners :: CV.Image.Image RGB D8 -> (Int, Int) -> [(Float,Float)] -> CV.Image.Image RGB D8
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

-- | See opencv function cvCalibrateCamera2. This function takes a list of world-screen coordinate pairs acquired with find-chessboard corners
--   and attempts to find the camera parameters for the system. It returns the fitting error, the camera matrix, list of distortion co-efficients
--   and rotation and translation vectors for each coordinate pair. 
calibrateCamera2 ::
     [[((Float, Float, Float), (Float, Float))]]
     -> (Int, Int)
     -> IO (Double, Matrix Float, [[Float]], [[Float]], [[Float]])
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

stereoRectifyUncalibrated :: Matrix (Float,Float) -> Matrix (Float,Float) -> Matrix Float -> (Int,Int) -> Double
                             -> (Matrix Float, Matrix Float)
stereoRectifyUncalibrated pts1 pts2 fund (w,h) threshold = unsafePerformIO $
    let h1 = emptyMatrix (3,3)
        h2 = emptyMatrix (3,3)
    in withMatPtr pts1     $ \c_pts1 -> 
       withMatPtr pts2     $ \c_pts2 ->
       withMatPtr fund     $ \c_fund -> 
       withMatPtr h1       $ \c_h1 -> 
       withMatPtr h2       $ \c_h2 -> 
       with (C'CvSize (fromIntegral w) (fromIntegral h)) $ \c_size -> do
        r <- c'wrapStereoRectifyUncalibrated c_pts1 c_pts2 c_fund c_size c_h1 c_h2 (realToFrac threshold)
        return (h1, h2)



findFundamentalMat :: Matrix Float -> Matrix Float -> CInt -> Double -> Double
                             -> Matrix Float --, Matrix Float)
findFundamentalMat pts1 pts2  method p1 p2 = unsafePerformIO $
    let fund = emptyMatrix (3,3)
    in withMatPtr pts1     $ \c_pts1 -> 
       withMatPtr pts2     $ \c_pts2 ->
       withMatPtr fund     $ \c_fund -> 
        do
         r <- c'cvFindFundamentalMat c_pts1 c_pts2 c_fund method (realToFrac p1) (realToFrac p2) nullPtr
         return (fund)
