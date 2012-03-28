{-#LANGUAGE ForeignFunctionInterface, ViewPatterns#-}
#include "cvWrapLEO.h"
module CV.LightBalance where

import Foreign.C.Types
import Foreign.Ptr

import System.IO.Unsafe
{#import CV.Image#}

f::Int -> CInt
f = fromIntegral
x2cylinder (f->w,f->h) m s c = unsafePerformIO $ creatingImage ({#call vignettingModelX2Cyl#} w h 
                            (realToFrac m) (realToFrac s) (realToFrac c))
cos4cylinder   (f->w,f->h) = unsafePerformIO $ creatingImage ({#call vignettingModelCos4XCyl#} w h)
cos4vignetting (f->w,f->h) = unsafePerformIO $ creatingImage ({#call vignettingModelCos4#} w h)
threeB (f->w,f->h) b1 b2 b3 = unsafePerformIO $ creatingImage ({#call vignettingModelB3#} w h b1 b2 b3)
twoPar (f->w,f->h) sx sy m = unsafePerformIO $ creatingImage ({#call vignettingModelP#} w h sx sy m)

