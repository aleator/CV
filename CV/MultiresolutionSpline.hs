-- | This module provides the elementary image splining (seamless merging) using the burt-adelson multiresolution splines
-- introduced in "A multiresolution spline with application to image mosaics", Burt, P.J. and Adelson, E.H., ACM Transactions on Graphics,1983.
module CV.MultiresolutionSpline where

import CV.Image
import qualified CV.ImageMath as IM
import CV.Transforms
import CV.ImageMathOp
import CV.Filters



-- | This function merges two images based on given mask, the first image dominates on areas where the mask
--   is 1 and the second where the mask is 0. The merging should be relatively seamless and is controlled by
--   the `levels` parameter, which adjusts the accuracy. Usually, decent results can be obtained with 4 pyramid 
--   levels.
--   
--   Note that the mask should contain a tiny blurred region between images for optimal result.
burtAdelsonMerge :: Int -> Image GrayScale D8 -> Image GrayScale D32 -> Image GrayScale D32 
                        -> Image GrayScale D32
burtAdelsonMerge levels mask img1 img2 
    | badSize = error $ "BAMerge: Images have a bad size. Not divisible by "++show divisor ++" "++show sizes 
    | otherwise = reconstructFromLaplacian pyrMerge
    where
        divisor = 2^levels
        notDivisible x = x`mod`(divisor) /= 0 
        sizes = map getSize [img1,img2]++[getSize mask]
        badSize = any (\(x,y) -> notDivisible x || notDivisible y) sizes
        maskPyr = reverse $ take levels $ iterate pyrDown $ mask
        pyr  = laplacianPyramid levels img1
        pyr2 = laplacianPyramid levels img2 
        pyrMerge = zipWith3 IM.maskedMerge maskPyr pyr2 pyr

