{-#LANGUAGE ViewPatterns#-}
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
<<<<<<< HEAD
        value (x,y) = getPixel (x,y) pixels
=======
        value (x,y) = getPixel (x,y) pixels 
>>>>>>> master
        (width,height) = getSize pixels

ntimes n op = (!! n) . iterate op

smooth [x] = [x]
smooth [x,y] = [(x+y) / 2]
smooth (x:y:xs) = (x+y) / 2:smooth (y:xs)

conv mask = map (prod mask) . tails
<<<<<<< HEAD
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
        rotated = rotate (pi/4.5) final
    saveImage "Combine.png" rotated
 where n = 5

nonMaxSupress w = reverse . nonMaxSupress' w . reverse . nonMaxSupress' w
nonMaxSupress' w = map supress . map (take w) . tails
    where
        supress [] = 0
        supress [x] = x
        supress (x:xs) | all (<x) xs = x
                       | otherwise   = 0
=======
    where 
     prod xs ys = sum (zipWith (*) xs ys)

-- TODO: Something is not type safe in this chain:
main = do
    Just x <- getArgs >>= loadImage . hdef (error "No file given!") >>= return . fmap (enlarge n )
    let 
        operR = unsafeImageTo32F . nibbly 1 0.01 . IM.abs . sobel (1,0) s3  -- nibbly 0.3 0.01
        operL = unsafeImageTo32F . nibbly 1 0.01 . IM.abs . sobel (0,1) s3  -- nibbly 0.3 0.01
        down oper = take n $ map oper $ iterate pyrDown x
        up  oper = foldl1 (\a b -> (pyrUp a) `IM.min` b) $ reverse (down oper)
        se = structuringElement (9,9) (4,4) EllipseShape
        thd =  dilate se 2 . unsafeImageTo32F . nibbly 0.9 0.001 $ IM.invert x 
        pyrd = stretchHistogram $  (up operL) #+ (up operR)-- $ sobel (1,0) s3 x -- $ (up operR)
        clean :: Image GrayScale D8
        clean = selectSizedComponents (10000) (10000000) $ IM.moreThan 0.2 $ thd #* (close se pyrd)
        final = unsafeImageTo32F clean -- #* (IM.abs (sobel (1,0) s1 x) #+ IM.abs (sobel (0,1) s1 x))
        rotated :: Image GrayScale D8
        rotated = rotate (pi/4.5) final
--        hm = (horMax rotated)
--        shm = ntimes 100 smooth hm
    saveImage "Combine.png" rotated
--    print (length hm,getSize rotated, getSize (fromImage rotated)) 
--    plotLists [YRange (-15,15)] [map (*100) $ nonMaxSupress 30 $ conv [-1,2,-1] $ shm, map (\x -> (x-400)/20) shm]
 where n = 5

nonMaxSupress w = reverse . nonMaxSupress' w . reverse . nonMaxSupress' w
nonMaxSupress' w = map supress . map (take w) . tails 
    where 
        supress [] = 0 
        supress [x] = x
        supress (x:xs) | all (<x) xs = x
                       | otherwise   = 0 
>>>>>>> master

threshold t x | x <t = 0
              | otherwise = 10
