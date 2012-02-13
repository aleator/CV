{-#LANGUAGE ScopedTypeVariables, ParallelListComp #-}
module Main where

import CV.Drawing
import CV.Filters
import CV.Edges
import CV.Image
import CV.ColourUtils
import CV.ImageMathOp
import CV.MultiresolutionSpline 
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import qualified CV.ImageMath as IM
import qualified CV.Transforms as T

merge :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
merge a b = (IM.invert mask #* b) #+ (mask #* a)
    where
     mask = unsafeImageTo32F $ (IM.abs a::Image GrayScale D32) #< (IM.abs b::Image GrayScale D32)

laplacianFusion a b = T.reconstructFromLaplacian $
               zipWith merge 
                (T.laplacianPyramid 5 a)
                (T.laplacianPyramid 5 b)

veskuFusion a b = (IM.invert mask #* b) #+ (mask #* a)
    where
     mask = unsafeImageTo32F $ (IM.abs (sobel (1,1) s5 a::Image GrayScale D32)) #> (IM.abs (sobel (1,1) s5 b::Image GrayScale D32))

sFuser i1 fn = do
    Just i2 <- loadImage fn >>= return . fmap (T.enlarge 5)
    let r = veskuFusion i1 i2
    r `seq` return r

laplacianImages img = [x T.pyrUp i $ l | l <- reverse (T.laplacianPyramid 5 img) | i <- [0..] ]
x a b = foldr (.) id (replicate b a)

demonstrate imgs = let
    gs =  [(no,oct,r) | (i,no)  <- zip imgs [1..]
            , (r,oct) <- zip (laplacianImages i) [1..]  
          ]
    sums = [(oct, IM.averageImages [r | (_,n,r) <- gs , n==oct ]) | oct <- [1..5]]
    in (gs,sums)

main = do
    (fn1:fns) <- getArgs 
    Just i1 <- loadImage fn1 >>= return . fmap (T.enlarge 5)
    is <- catMaybes <$> mapM (loadImage  >=> return . fmap (T.enlarge 5)) (fn1:fns)
    r <- foldM sFuser i1 fns
    saveImage "fusing_result.png" $ stretchHistogram r
    let (ls,ss) = (demonstrate is)
    sequence [saveImage ("avg"++show oct++".png") $ balance (0.3, 0.3) r
             | ((oct,r)) <- ss | i <- [1..]]
    sequence [saveImage ("laplacian"++show no++"_"++show oct++".png") $ balance (0.3, 0.3) r
             | ((no,oct,r)) <- ls | i <- [1..]]


