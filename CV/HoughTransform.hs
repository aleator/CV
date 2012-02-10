{-#LANGUAGE ScopedTypeVariables#-}
module CV.HoughTransform where

import CV.Bindings.ImgProc
import CV.Matrix
import CV.Image (withImage, D8, GrayScale, Image)
import System.IO.Unsafe
import Foreign.Ptr


houghLinesStandard :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Matrix (Float,Float)
houghLinesStandard img n ρ θ t = unsafePerformIO $ do
    let m :: Matrix (Float,Float) = create (1,n)
    withMatPtr m $ \c_m -> 
        withImage img $ \c_img -> 
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_STANDARD ρ θ t 0 0
    return m

houghLinesProbabilistic :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> Matrix (Int,Int,Int,Int)
houghLinesProbabilistic img n ρ θ t minLength maxGap = unsafePerformIO $ do
    let m :: Matrix (Int,Int,Int,Int) = create (1,n)
    withMatPtr m $ \c_m -> 
        withImage img $ \c_img -> 
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_PROBABILISTIC ρ θ t minLength maxGap
    return m

houghLinesMultiscale :: Image GrayScale D8 -> Int -> Double -> Double -> Int -> Double -> Double
                                -> Matrix (Float,Float)
houghLinesMultiscale img n ρ θ t distDiv angleDiv = unsafePerformIO $ do
    let m :: Matrix (Float,Float) = create (1,n)
    withMatPtr m $ \c_m -> 
        withImage img $ \c_img -> 
            c'cvHoughLines2 (castPtr c_img) (castPtr c_m) c'CV_HOUGH_MULTI_SCALE ρ θ t distDiv angleDiv
    return m
