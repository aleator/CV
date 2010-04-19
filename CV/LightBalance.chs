{-#LANGUAGE ForeignFunctionInterface#-}
#include "cvWrapLEO.h"
module CV.LightBalance where

import Foreign.C.Types
import Foreign.Ptr

import C2HSTools
{#import CV.Image#}

x2cylinder (w,h) m s c = unsafePerformIO $ creatingImage ({#call vignettingModelX2Cyl#} w h 
                            (realToFrac m) (realToFrac s) (realToFrac c))
cos4cylinder   (w,h) = unsafePerformIO $ creatingImage ({#call vignettingModelCos4XCyl#} w h)
cos4vignetting (w,h) = unsafePerformIO $ creatingImage ({#call vignettingModelCos4#} w h)
threeB (w,h) b1 b2 b3 = unsafePerformIO $ creatingImage ({#call vignettingModelB3#} w h b1 b2 b3)
twoPar (w,h) sx sy m = unsafePerformIO $ creatingImage ({#call vignettingModelP#} w h sx sy m)

