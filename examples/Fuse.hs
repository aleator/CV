{-#LANGUAGE ScopedTypeVariables, ParallelListComp #-}
module Main where

import CV.Drawing
import CV.Filters
import CV.Image
import CV.ColourUtils
import CV.ImageMathOp
import CV.Pixelwise
import CV.MultiresolutionSpline
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import qualified CV.ImageMath as IM
import qualified CV.Transforms as T

merge :: D32 -> D32 -> D32
merge a b | abs a < abs b = b
          | otherwise     = a

laplacianFusion :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
laplacianFusion a b = T.reconstructFromLaplacian $
               [toImage $ merge <$$> x <+> y
               | x <- T.laplacianPyramid 5 a
               | y <- T.laplacianPyramid 5 b]

sFuser i1 fn = do
    Just i2 <- loadImage fn >>= return . fmap (T.enlarge 5)
    let r = laplacianFusion i1 i2
    r `seq` return r


main = do
    (fn1:fns) <- getArgs
    Just i1 <- loadImage fn1 >>= return . fmap (T.enlarge 5)
    r <- foldM sFuser i1 fns
    saveImage "fusing_result.png" $ stretchHistogram r


