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
burtAdelsonMerge levels mask img1 img2 
    | badSize = error $ "BAMerge: Images have a bad size. Not divisible by "++show divisor ++" "++show sizes 
    | otherwise = reconstructFromLaplacian pyrMerge
    where
        divisor = 2^levels
        notDivisible x = x`mod`(divisor) /= 0 
        sizes = map getSize [mask,img1,img2]
        badSize = any (\(x,y) -> notDivisible x || notDivisible y) sizes
        maskPyr = reverse $ take levels $ iterate pyrDown $ mask
        pyr  = laplacianPyramid levels img1
        pyr2 = laplacianPyramid levels img2 
        pyrMerge = zipWith3 IM.maskedMerge maskPyr pyr2 pyr

-- | Another Burt-Adelson spline. Since OpenCV:s pyramids are inflexible, this one does without
--   Naturally, this is much more inefficient and must be seen as stop-gap solution for splining
--   arbitrary size images. Does not work, btw.

burtAdelsonMerge2 levels mask img1 img2 = foldl1 (#+) pyrMerge
    where
        g = gaussian (25,25)
        fakeLaplacian gpyr = zipWith (#-) gpyr (tail gpyr) ++ last [gpyr]
        maskPyr = take levels $ iterate g mask
        gpyr1 = take levels . iterate g $ img1
        gpyr2 = take levels . iterate g $ img2
        pyrMerge = zipWith3 IM.maskedMerge maskPyr (fakeLaplacian gpyr2) (fakeLaplacian gpyr1)


