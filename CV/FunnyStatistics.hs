module CV.FunnyStatistics where

import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp

--nthCM s n i = blur s $ (i #- blur s i) |^ n

r_variance s i = msq #- (m #* m) 
        where
            msq = gaussian s (i #* i)
            m = gaussian s i

variance s i = msq #- (m #* m) 
        where
            msq = blur s (i #* i)
            m = blur s i

stdDev s i = IM.sqrt $ variance s i
r_stdDev s i = IM.sqrt $ r_variance s i

{-
skewness s i = IM.div (nthCM s 3 i) (stdDev s i |^3)

kurtosis s i = IM.div (nthCM s 4 i) (stdDev s i |^4)
xx s i = IM.div (nthCM s 6 i) (stdDev s i |^6)
                                  -}

pearsonSkewness1 s image = IM.div (blur s image #- unsafeImageTo32F (median s (unsafeImageTo32F image))) 
                                  (stdDev s image)
