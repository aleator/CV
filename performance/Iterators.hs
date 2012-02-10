module Main where
import Criterion.Main 
import CV.Image
import qualified CV.ImageMath as IM
import System.IO.Unsafe
import CV.ColourUtils
import CV.Pixelwise
import CV.Iterators
import qualified CV.Transforms as T

main = do
        Just x <-  loadImage "smallLena.jpg" 
        let iter n = length $ filterPixels (>n) x
            slow n = length $ filterPixelsSlow (>n) x
            comp n = length $ [((i,j),p) | i<- [0..w-1] , j <- [0..h-1], let p = getPixel (i,j) x, p>n]
            (w,h) = getSize x
        print (iter 0.5,slow 0.5, comp 0.5)
        defaultMain [
         bgroup "iterator" [
               bench "huge"   $ nf iter 0.3 
              ,bench "medium" $ nf iter 0.8 
              ,bench "small"  $ nf iter 0.9
         
         ,bgroup "list-comp" [
               bench "huge"   $ nf comp 0.3 
              ,bench "medium" $ nf comp 0.8 
              ,bench "small"  $ nf comp 0.9
                            ]
         ,bgroup "filter-pixels-slow" [
               bench "huge"   $ nf comp 0.3 
              ,bench "medium" $ nf comp 0.8 
              ,bench "small"  $ nf comp 0.9
                            ]

         ]]

