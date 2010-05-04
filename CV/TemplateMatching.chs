{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.TemplateMatching where

import Foreign.C.Types
import Foreign.Ptr

import CV.Image
import CV.Transforms

import Utils.Function
import Utils.Point
import Utils.Rectangle hiding (scale)

{#import CV.Image#}
import C2HSTools

getTemplateMap image template = unsafePerformIO $
	   withImage image $ \cvimg ->
	    withImage template $ \cvtemp ->
         creatingImage $ {#call templateImage#} cvimg cvtemp
         

-- TODO: Make this somehow smarter #PotentialDanger
data MatchType = CV_TM_SQDIFF | CV_TM_SQDIFF_NORMED | CV_TM_CCORR
                 | CV_TM_CCORR_NORMED | CV_TM_CCOEFF | CV_TM_CCOEFF_NORMED 
                 deriving (Eq,Show,Enum)


simpleTemplateMatch :: MatchType -> Image -> Image -> ((CInt,CInt),Double)
simpleTemplateMatch mt image template 
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
		    return ((x,y),realToFrac v); }

matchTemplate :: MatchType-> Image -> Image -> Image
matchTemplate mt image template = unsafePerformIO $ do
     let isize = getSize image
         tsize = getSize template
         size  = isize - tsize + (1,1) 
     res <- createImage32F size 1 
     withGenImage image $ \cimg -> 
      withGenImage template $ \ctempl ->
       withGenImage res $ \cresult -> 
        {#call cvMatchTemplate#} cimg ctempl cresult (fromIntegral . fromEnum $ mt)
     return res


-- | Perform subpixel template matching using intensity interpolation
-- TODO: CDouble to Double #CleanUp
subPixelTemplateMatch :: MatchType -> Image -> Image -> CDouble -> (CDouble,CDouble)
subPixelTemplateMatch mt image template n -- TODO: Make iterative #SpeedUp
    = (fromIntegral (tx)+fromIntegral sbx/n 
      ,fromIntegral (ty)+fromIntegral sby/n)
     where
        (otw,oth) = getSize template
        ((orX,orY),_) = simpleTemplateMatch CV_TM_CCORR_NORMED image template
        (tx,ty) = (orX-otw`div`2, orY-oth`div`2)

        bigTempl = scale Linear n template
        (tw,th) = getSize bigTempl
        region = scale Linear n . getRegion (tx,ty) (otw*2,oth*2)  $ image
        ((sbx,sby),_) = simpleTemplateMatch CV_TM_CCORR_NORMED region bigTempl
     
regionToInt rec = mkRectangle (floor x,floor y) (ceiling w,ceiling h)
    where
     (x,y) = topLeft rec
     (w,h) = rSize rec
