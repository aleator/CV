{-#LANGUAGE ScopedTypeVariables, TypeOperators#-}
module CV.HoughTransform where

import CV.Bindings.ImgProc
import CV.Matrix
import CV.Image (withImage, D8, GrayScale, Image, getSize)
import System.IO.Unsafe
import Foreign.C.Types
import Foreign.Ptr

type HoughDesc = Int

data Segment = Segment
  { start :: (Int,Int)
  , end :: (Int,Int)
  } deriving (Eq,Show)

data Line = Line
   {bias :: Double
   ,θ    :: Double
   } deriving (Eq,Show)

data With a b = a `With` b

type ImageWithLines    = Image GrayScale D8 `With` [Line]
type ImageWithSegments = Image GrayScale D8 `With` [Segment]

image :: Image c d `With` e -> Image c d
image (a `With` _) = a

lines :: a `With` [Line] -> [Line]
lines (_ `With` b) = b

segments :: a `With` [Segment] -> [Segment]
segments (_ `With` b) = b

lineToSegment (w,h) (Line y k) = (Segment (0,round y)
                                       (w,round $ y + (fromIntegral w)*negate (tan (pi/2-k))))

houghProbabilisticToLine d (x,y) (u,v) = Segment (x,y) (u,v)

rho1pix :: Double = 1.0
rho5pix :: Double = 5
theta1deg :: Double = pi/180
theta2deg :: Double = pi/90

imageHoughLinesStandard :: Int -> Double -> Double -> Int -> Image GrayScale D8 -> ImageWithLines
imageHoughLinesStandard n ρ θ t img =
  (img `With` [Line (realToFrac y) (realToFrac k) | (y,k) <- hough])
  where
    hough = houghLinesStandard img n ρ θ t

imageHoughLinesProbabilistic :: Int -> Double -> Double -> Int -> Double -> Double -> Image GrayScale D8 -> ImageWithSegments
imageHoughLinesProbabilistic n ρ θ t minLength maxGap img =
  (img `With` [Segment (fromIntegral x,fromIntegral y) (fromIntegral u,fromIntegral v) | (x,y,u,v) <- hough])
  where
        (w,h) = getSize img
        hough = houghLinesProbabilistic img n ρ θ t minLength maxGap

imageHoughLinesMultiScale :: Int -> Double -> Double -> Int -> Double -> Double -> Image GrayScale D8 -> ImageWithLines
imageHoughLinesMultiScale n ρ θ t distDiv angleDiv img =
  (img `With` [Line (realToFrac y)(realToFrac k) | (y,k) <- hough])
  where
        (w,h) = getSize img
        hough = houghLinesMultiscale img n ρ θ t distDiv angleDiv

houghLinesStandard :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> [(CFloat,CFloat)]
houghLinesStandard img n ρ θ t = unsafePerformIO $ do
    m :: Matrix (CFloat,CFloat) <- create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_STANDARD ρ θ t 0 0
    return $ toList m

houghLinesProbabilistic :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> [(CInt,CInt,CInt,CInt)]
houghLinesProbabilistic img n ρ θ t minLength maxGap = unsafePerformIO $ do
    m :: Matrix (CInt,CInt,CInt,CInt) <- create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_PROBABILISTIC ρ θ t minLength maxGap
    return $ toList m

houghLinesMultiscale :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> [(CFloat,CFloat)]
houghLinesMultiscale img n ρ θ t distDiv angleDiv = unsafePerformIO $ do
    m :: Matrix (CFloat,CFloat) <- create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_MULTI_SCALE ρ θ t distDiv angleDiv
    return $ toList m

houghCirclesGradient :: Image GrayScale D8 -> Int -> Double -> Double -> Double -> Double -> Int -> Int -> [(CFloat, CFloat, CFloat)]
houghCirclesGradient img n dp minDist cannyThresh accumThresh minRad maxRad = unsafePerformIO $ do
    m :: Matrix (CFloat,CFloat,CFloat) <- create (1,n)
    withMatPtr m $ \c_m ->
        withImage img $ \c_img ->
            c'cvHoughCircles (castPtr c_img) (castPtr c_m) c'CV_HOUGH_GRADIENT dp minDist cannyThresh accumThresh minRad maxRad
    return $ toList m
