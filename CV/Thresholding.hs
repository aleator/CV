module CV.Thresholding
where
import CV.Image
import CV.Filters
import qualified CV.ImageMath as IM
import CV.ImageMathOp
import CV.Morphology
import System.IO.Unsafe
import CV.Sampling
import Utils.List
import Data.List
import CV.Histogram


bernsen (w,h) c i = goodContrast #* (i #< surface)
            where
             low  = erode se 1  i
             high = dilate se 1 i
             goodContrast = IM.moreThan c (high #- low)
             surface = 0.5 |* (high #+ low) 
             se = structuringElement (w,h) (w`div`2,h`div`2) EllipseShape

-- Very slow implementation of niblack thresholding
--niblack (w,h) k i = IM.more2Than trunc (unsafePerformIO $ surface) 
--    where
--     trunc = getRegion (w`div`2,h`div`2) (wi-w,hi-h) i
--     (wi,hi) = getSize i
--     surface = renderFlatList (wi-w,hi-h) (map th patches)
--     th ptch = IM.average ptch + k * IM.stdDeviation ptch
--     patches = allPatches (w,h) i

nibbly k c i = let dev = IM.stdDeviation i
                   mean = IM.average i 
             in IM.moreThan (mean+k*dev+c) i

nibblyr (w,h) k i = IM.lessThan t flat
    where
     t = IM.average flat + k * IM.stdDeviation flat
     flat = i #- gaussian (w,h) i


-- TODO: Convert Histograms from Doubles to Floats..
otsu bs image = IM.moreThan (realToFrac threshold) image
    where
        histogram  = getHistogram bs $ image
        partitions = histogramPartitions histogram 
        (threshold,_,_) = maximumBy (comparing otsuCmp) partitions 
        otsuCmp (t,as,bs) = betweenClassVariance (as) (bs)

-- This is excruciatingly slow means of finding kittler-illingworth threshold
-- for an image
kittler precision image = IM.moreThan t image
    where t = maximumBy (comparing (kittlerMeasure image))
                            [0,0+precision..1]

kittlerMeasure image t = unNaN $ 
                        p_t*log fgDev
                      + (1-p_t)*log bgDev
                      - p_t*log p_t 
                      - (1-p_t)*log(1-p_t)
    where
     unNaN x | isNaN x = -10000000
             | otherwise = x
     thresholded = unsafeImageTo32F (IM.lessThan t image)
     p_t = IM.sum ( thresholded) / fromIntegral (getArea image)
     bgDev = realToFrac $ IM.stdDeviationMask image thresholded 
     fgDev = realToFrac $ IM.stdDeviationMask image (IM.invert thresholded)


histogramPartitions (HGD a) = zip3 (head.tails.map fst $ a) 
                                   (tail.inits.map snd $ a) 
                                   (reverse.tail.reverse.tails.map snd $ a)

betweenClassVariance as bs = sum as * sum bs 
                             * (average bs - average as)^2

