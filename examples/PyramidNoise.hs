{-#LANGUAGE ScopedTypeVariables #-}
module Main where

import CV.Image
import CV.MultiresolutionSpline 
import CV.ImageOp
import CV.Drawing
import CV.Filters
import CV.ImageMathOp
import Control.Applicative
import Data.Maybe
import qualified CV.ImageMath as IM
import CV.Transforms
import System.Environment

absTresh :: D32 -> Image GrayScale D32 -> Image GrayScale D32 
absTresh t a = mask #* a
    where
     mask = unsafeImageTo32F $ t |> (IM.abs a::Image GrayScale D32) 

main = do
    [fn] <- getArgs 
    Just img <- loadImage fn >>= return.fmap (enlarge 4)
    let reduce = reconstructFromLaplacian . map (absTresh 0.1) . laplacianPyramid 4
    saveImage "denoising_result.png" $ montage (1,2) 2 $  [img,reduce img]



