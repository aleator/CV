module CV.MultiresolutionSpline where

import CV.Image
import qualified CV.ImageMath as IM
import CV.Transforms
import CV.ImageMathOp
import CV.Filters


-- stitchHalfAndHalf i1 i2 = montage (2,1) 0 [getRegion (0,0) (hw,dh) i1,getRegion (hw,0) (hw,dh) i2]
--     where
--      dh = h
--      (w,h) = getSize i1
--      (hw,hh) = (w`div`2,h`div`2)

-- | Do a burt-adelson multiresolution splining for two images.
--   Notice, that the mask should contain a tiny blurred region between images 
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

