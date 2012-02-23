{-#LANGUAGE ScopedTypeVariables#-}
module CV.HoughTransform where

import CV.Bindings.ImgProc
import CV.Matrix
import CV.Image (withImage, D8, GrayScale, Image, getSize)
import System.IO.Unsafe
import Foreign.C.Types
import Foreign.Ptr

type HoughDesc = Int

data Line d =
  Line
  { start :: (Int,Int)
  , end :: (Int,Int)
  , desc :: d
  }

data ImageWithLines d =
  ImageWithLines
  { image :: Image GrayScale D8
  , lines :: [Line d]
  }

houghToLine d (w,h) (y,k) = (Line (0,round y) (w,round $ y + (fromIntegral w)*negate (tan (pi/2-k))) d)

houghProbabilisticToLine d (x,y) (u,v) = (Line (x,y) (u,v) d)

rho1pix :: Double = 1.0
rho5pix :: Double = 5
theta1deg :: Double = pi/180
theta2deg :: Double = pi/90

imageHoughLinesStandard :: Int -> Double -> Double -> Int -> Image GrayScale D8 -> ImageWithLines HoughDesc
imageHoughLinesStandard n ρ θ t img =
  (ImageWithLines img [houghToLine 0 (w,h) (realToFrac y,realToFrac k) | (y,k) <- hough])
  where
    (w,h) = getSize img
    hough = houghLinesStandard img n ρ θ t

imageHoughLinesProbabilistic :: Int -> Double -> Double -> Int -> Double -> Double -> Image GrayScale D8 -> ImageWithLines HoughDesc
imageHoughLinesProbabilistic n ρ θ t minLength maxGap img =
  (ImageWithLines img [houghProbabilisticToLine 0 (fromIntegral x,fromIntegral y) (fromIntegral u,fromIntegral v) | (x,y,u,v) <- hough])
  where
        (w,h) = getSize img
        hough = houghLinesProbabilistic img n ρ θ t minLength maxGap

imageHoughLinesMultiScale :: Int -> Double -> Double -> Int -> Double -> Double -> Image GrayScale D8 -> ImageWithLines HoughDesc
imageHoughLinesMultiScale n ρ θ t distDiv angleDiv img =
  (ImageWithLines img [houghToLine 0 (w,h) (realToFrac y,realToFrac k) | (y,k) <- hough])
  where
        (w,h) = getSize img
        hough = houghLinesMultiscale img n ρ θ t distDiv angleDiv

houghLinesStandard :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> [(CFloat,CFloat)]
houghLinesStandard img n ρ θ t = unsafePerformIO $ do
    let m :: Matrix (CFloat,CFloat) = create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_STANDARD ρ θ t 0 0
    return $ toList m

houghLinesProbabilistic :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> [(CInt,CInt,CInt,CInt)]
houghLinesProbabilistic img n ρ θ t minLength maxGap = unsafePerformIO $ do
    let m :: Matrix (CInt,CInt,CInt,CInt) = create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_PROBABILISTIC ρ θ t minLength maxGap
    return $ toList m

houghLinesMultiscale :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> [(CFloat,CFloat)]
houghLinesMultiscale img n ρ θ t distDiv angleDiv = unsafePerformIO $ do
    let m :: Matrix (CFloat,CFloat) = create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_MULTI_SCALE ρ θ t distDiv angleDiv
    return $ toList m
