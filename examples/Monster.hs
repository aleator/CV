{-#LANGUAGE ViewPatterns, ParallelListComp#-}
module Main where

import CV.Image

import CV.ColourUtils
import CV.Morphology
import CV.Filters
import CV.Thresholding
import CV.Edges
import CV.ImageMathOp
import qualified CV.ImageMath as IM
import CV.FunnyStatistics
import CV.ConnectedComponents
import CV.Transforms
import CV.Matrix
import CV.Fitting
import System.Environment
import CV.Pixelwise
import Graphics.Gnuplot.Simple
import Data.List
import Data.Ord

hdef d (x:xs) = x
hdef d _      = d

horMax :: Image GrayScale D32 -> [Double]
horMax (pixels) = [fromIntegral $ snd $ maximumBy (comparing value) [(i,j) | j<-[0..height-1]] | i <- [0..width-1] ]
    where
        value (x,y) = getPixel (x,y) pixels
        value (x,y) = getPixel (x,y) pixels
        (width,height) = getSize pixels

getMarks :: Image GrayScale D32 -> [(Float,Float)]
getMarks pixels = [(realToFrac i,realToFrac j) | j <- [0..height-1]
                         , i <- [0..width-1]
                         , value (i,j) > 0.5]
    where
        value (x,y) = getPixel (x,y) pixels
        value (x,y) = getPixel (x,y) pixels
        (width,height) = getSize pixels

ntimes n op = (!! n) . iterate op

smooth [x] = [x]
smooth [x,y] = [(x+y) / 2]
smooth (x:y:xs) = (x+y) / 2:smooth (y:xs)

conv mask = map (prod mask) . tails
    where
     prod xs ys = sum (zipWith (*) xs ys)

to32F = unsafeImageTo32F

main = do
    Just x <- getArgs >>= loadImage . hdef (error "No file given!") >>= return . fmap (enlarge n )
    let
        oper msk = to32F . nibbly 1 0.01 . IM.abs . sobel msk s3
        up op = foldl1 (\a b -> (pyrUp a) `IM.min` b)
                 . reverse
                 . take n
                 . map op
                 . iterate pyrDown

        se = structuringElement (9,9) (4,4) EllipseShape
        pyrd x =  (up (oper (1,0)) x) #+ (up (oper (0,1)) x)

        thd =  dilate se 2 . unsafeImageTo32F . nibbly 0.9 0.001 . IM.invert 
        clean = selectSizedComponents (1e4) (1e10) . IM.moreThan 0.2 $ thd x #* (close se (pyrd x))

        final = to32F clean #* (IM.abs (sobel (1,0) s1 x) #+ IM.abs (sobel (0,1) s1 x))
        hm = getMarks $ to32F clean
        ell = fitEllipse $ fromList (1,length hm) $ hm
        rotated = rotate (realToFrac $ pi/2+2*pi*(angle ell/360)) final
    saveImage "Combine.png" rotated
 where n = 5

nonMaxSupress w = reverse . nonMaxSupress' w . reverse . nonMaxSupress' w
nonMaxSupress' w = map supress . map (take w) . tails
    where
        supress [] = 0
        supress [x] = x
        supress (x:xs) | all (<x) xs = x
                       | otherwise   = 0

threshold t x | x <t = 0
              | otherwise = 10
