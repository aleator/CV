{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.Histogram where

import CV.Image
{#import CV.Image#}

import Data.List
import Data.Array
import Data.Array.ST
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import C2HSTools

import System.IO.Unsafe

-- import Utils.List

newtype Num a => HistogramData a = HGD [(a,a)]

-- Assume [0,1] distribution and calculate skewness
skewness bins image = do
                 hg <- buildHistogram cbins image
                 bins <-  mapM (getBin hg) [0..cbins-1]
                 let avg = sum bins / (fromIntegral.length) bins
                 let u3 = sum.map (\(value,bin) -> 
                                     (value-avg)*(value-avg)*(value-avg)
                                     *bin) $
                            zip binValues bins 
                 let u2 = sum.map (\(value,bin) -> 
                                     (value-avg)*(value-avg)
                                     *bin) $
                            zip binValues bins 
                
                 return (u3 / (sqrt u2*sqrt u2*sqrt u2))
                where
                 cbins :: CInt
                 cbins = fromIntegral bins
                 binValues = [0,fstep..1]
                 fstep = 1/(fromIntegral bins)

values (HGD a) = snd.unzip $ a

-- This does not make any sense!
cmpUnion a b = sum $ zipWith (max) a b

cmpIntersect a b = sum $ zipWith min a b

cmpEuclidian a b = sum $ (zipWith (\x y -> (x-y)^2) a b)
cmpAbs a b = sum $ (zipWith (\x y -> abs (x-y)) a b)

chiSqrHG  a b = chiSqr (values a) (values b) 
chiSqr a b = sum $ zipWith (calc) a b
    where
     calc a b = (a-b)*(a-b) `divide` (a+b)
     divide a b | abs(b) > 0.000001 = a/b
                | otherwise = 0

liftBins op (HGD a) = zip (op bins) values
            where (bins,values) = unzip a

liftValues op (HGD a) = zip bins (op values)
            where (bins,values) = unzip a

sub (HGD a) (HGD b) | bins a == bins b 
                    = HGD $ zip (bins a) values
                where
                 bins a = map fst a
                 msnd = map snd
                 values = zipWith (-) (msnd a) (msnd b)
              

noBins (HGD a) = length a

getPositivePart (HGD a) = HGD $ dropWhile ((<0).fst) a
tcumulate [] = []
tcumulate values = tail $ scanl (+) 0 values

getCumulativeNormalHistogram binCount image 
    = HGD $ zip bins $ tcumulate values
    where
        HGD lst = getNormalHistogram binCount image
        bins :: [CDouble]
        values :: [CDouble]
        (bins,values) = unzip lst

weightedHistogram img weights start end binCount = unsafePerformIO $ 
    withImage img $ \i -> 
     withImage weights $ \w -> do
      bins <- mallocArray (fromIntegral binCount)
      {#call get_weighted_histogram#} i w (realToFrac start) 
                                          (realToFrac end) 
                                          (fromIntegral binCount) bins
      r <- peekArray binCount bins >>= return.map realToFrac
      free bins
      return r

simpleGetHistogram img mask start end binCount cumulative = unsafePerformIO $
    withImage img $ \i -> do
      bins <- mallocArray binCount    
      let isCum | cumulative == True  = 1
                | cumulative == False = 0
                
      case mask of
        (Just msk) -> do
                   mask8 <- (imageTo8Bit msk)
                   withImage mask8 $ \m -> do
                    {#call get_histogram#} i m (realToFrac start) (realToFrac end) 
                                               isCum (fromIntegral binCount) bins
        Nothing  -> {#call get_histogram#} i (nullPtr) 
                                             (realToFrac start) (realToFrac end) 
                                             isCum (fromIntegral binCount) bins

      r <- peekArray binCount bins >>= return.map realToFrac
      free bins
      return r        

       
       
        
getNormalHistogram bins image = HGD new
    where
        (HGD lst) = getHistogram bins image 

        value :: [CDouble]
        bin   :: [CDouble]
        (bin,value) = unzip lst
        new = zip bin $ map (/size) value
        size = fromIntegral $ uncurry (*) $ getSize image

getHistogram :: Int -> Image -> HistogramData CDouble
getHistogram bins image = unsafePerformIO $ do 
                            h <- buildHistogram cbins image 
                            values <- mapM (getBin h) 
                                        [0..fromIntegral bins-1] 
                            return.HGD $ 
                                zip [-1,-1+2/(fromIntegral bins)..1] values
                        where
                         cbins = fromIntegral bins


getHistgramHS bins image =  calcHistogram bins $ getAllPixels image

-- Calculate image histogram from _Floating Point_ Image
calcHistogram :: Int -> [CDouble] -> HistogramData Double
calcHistogram bins pixels = HGD $ map (\(a,b) -> (realToFrac a, b/l)) $ assocs $ accumArray (+) 0 (0,bins) binned 
                    where
                     l = fromIntegral $ length pixels
                     bin :: CDouble -> (Int,Double)
                     bin d = (floor $ (fromIntegral bins) * d,1.0)
                     binned = map bin pixels

-- Low level interaface:

{#pointer *CvHistogram as Histogram foreign newtype#}

foreign import ccall "& wrapReleaseHist" releaseHistogram :: FinalizerPtr Histogram
creatingHistogram fun = do
              iptr <- fun
              fptr <- newForeignPtr releaseHistogram iptr
              return.Histogram $ fptr

buildHistogram bins image = withGenImage image $ \ i ->
                       creatingHistogram 
                        ({#call calculateHistogram#} i bins)

getBin :: Histogram -> CInt -> IO CDouble
getBin hist bin = withHistogram hist $ \h ->
                    ({#call getHistValue#} h bin)
