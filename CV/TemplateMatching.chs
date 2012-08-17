{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.TemplateMatching where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import CV.Image
import CV.Transforms

import Utils.Function
import Utils.Point
import Utils.Rectangle hiding (scale)

{#import CV.Image#}
import System.IO.Unsafe
import Control.Exception

getTemplateMap image template = unsafePerformIO $
	   withImage image $ \cvimg ->
	    withImage template $ \cvtemp ->
         creatingImage $ {#call templateImage#} cvimg cvtemp
         

#c
enum MatchType {
   SQDIFF        = CV_TM_SQDIFF
  ,SQDIFF_NORMED = CV_TM_SQDIFF_NORMED
  ,CCORR         = CV_TM_CCORR
  ,CCORR_NORMED  = CV_TM_CCORR_NORMED
  ,CCOEFF        = CV_TM_CCOEFF
  ,CCOEFF_NORMED = CV_TM_CCOEFF_NORMED
};
#endc
{#enum MatchType {}#}


simpleTemplateMatch :: MatchType -> Image GrayScale D32 -> Image GrayScale D32 -> ((Int,Int),Double)
simpleTemplateMatch mt image template 
 | image `biggerThan` template
	= unsafePerformIO $ do
	   withImage image $ \cvimg ->
	    withImage template $ \cvtemp ->
	     alloca $ \(ptrintx :: Ptr CInt) ->
	      alloca $ \(ptrinty :: Ptr CInt)->
	       alloca $ \(ptrdblval :: Ptr CDouble) -> do {
	        {#call simpleMatchTemplate#} cvimg cvtemp ptrintx ptrinty ptrdblval (fromIntegral $ fromEnum mt);
		    x <- peek ptrintx;
			y <- peek ptrinty;
			v <- peek ptrdblval;
		    return ((fromIntegral x,fromIntegral y),realToFrac v); }
 | otherwise = throw $ CvSizeError "simpleTemplateMatch: template is bigger than the image"

matchTemplate :: MatchType-> Image GrayScale D32 -> Image GrayScale D32 -> Image GrayScale D32 
matchTemplate mt image template 
 | image `biggerThan` template = unsafePerformIO $ do
     let isize = getSize image
         tsize = getSize template
         size  = isize - tsize + (1,1) 
     res <- create size 
     withGenImage image $ \cimg -> 
      withGenImage template $ \ctempl ->
       withGenImage res $ \cresult -> 
        {#call cvMatchTemplate#} cimg ctempl cresult (fromIntegral . fromEnum $ mt)
     return res
 | otherwise = throw $ CvSizeError $ "MatchTemplate: template larger than the image"


-- | Perform subpixel template matching using intensity interpolation
subPixelTemplateMatch :: MatchType -> Image GrayScale D32 -> Image GrayScale D32 -> Double -> (Double,Double)
subPixelTemplateMatch mt image template n -- TODO: Make iterative #SpeedUp
    = (fromIntegral (tx)+fromIntegral sbx/n 
      ,fromIntegral (ty)+fromIntegral sby/n)
     where
        (otw,oth) = getSize template
        ((orX,orY),_) = simpleTemplateMatch CCORR_NORMED image template
        (tx,ty) = (orX-otw`div`2, orY-oth`div`2)

        bigTempl = scaleSingleRatio Linear n template
        (tw,th) = getSize bigTempl
        region = scaleSingleRatio Linear n . getRegion (tx,ty) (otw*2,oth*2)  $ image
        ((sbx,sby),_) = simpleTemplateMatch CCORR_NORMED region bigTempl
     
regionToInt rc = mkRectangle (floor x,floor y) (ceiling w,ceiling h)
    where
        (x,y) = topLeft rc
        (w,h) = rSize rc

#c
enum ShapeMatchMethod {
  Method1 = CV_CONTOURS_MATCH_I1,
  Method2 = CV_CONTOURS_MATCH_I2,
  Method3 = CV_CONTOURS_MATCH_I3
};
#endc
{#enum ShapeMatchMethod {}#}


-- | Match shapes
matchShapes :: ShapeMatchMethod -> Image GrayScale D8 -> Image GrayScale D8 -> Double
matchShapes m a b = unsafePerformIO $ do
    withGenImage a $ \c_a ->
     withGenImage b $ \c_b ->
       {#call cvMatchShapes#} c_a c_b (fromIntegral . fromEnum $ m) 0 
        >>= return.realToFrac
