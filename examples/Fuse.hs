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
import qualified CV.Transforms as T
import System.Environment

merge :: Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32
merge a b = (IM.invert mask #* a) #+ (mask #* b)
    where
     mask = unsafeImageTo32F $ (IM.abs a::Image GrayScale D32) #< (IM.abs b::Image GrayScale D32)

laplacianFusion a b = T.reconstructFromLaplacian $
               zipWith merge 
                (T.laplacianPyramid 5 a)
                (T.laplacianPyramid 5 b)

main = do
    env <- getArgs 
    imgs <- mapM loadImage env  >>= return. map (fromJust . (fmap (T.enlarge 5)))
    let r = foldl1 laplacianFusion imgs
    saveImage "fusing_result.png" $ montage (1,1+length imgs) 2 $  r:imgs


