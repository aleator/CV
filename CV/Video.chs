{-#LANGUAGE ForeignFunctionInterface, ViewPatterns#-}
#include "cvWrapLeo.h"
module CV.Video where
{#import CV.Image#}

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

{#pointer *CvCapture as Capture foreign newtype#}

foreign import ccall "& wrapReleaseCapture" releaseCapture :: FinalizerPtr Capture
-- NOTE: This use of foreignPtr is quite likely to cause trouble by retaining
--       videos longer than necessary.

captureFromFile fn = withCString fn $ \cfn -> do
                      ptr <- {#call cvCreateFileCapture#} cfn
                      fptr <- newForeignPtr releaseCapture ptr
                      return . Capture $ fptr

dropFrame cap = withCapture cap $ \ccap -> {#call cvGrabFrame#} ccap >> return ()

getFrame cap = withCapture cap $ \ccap -> do
                p_frame <- {#call cvQueryFrame#} ccap 
                creatingImage $ ensure32F p_frame -- NOTE: This works because Image module has generated wrappers for ensure32F

cvCAP_PROP_FRAME_COUNT = 7
cvCAP_POS_FRAMES = 1
getNumberOfFrames cap = unsafePerformIO $
                         {#call cvGetCaptureProperty#} 
                           cap cvCAP_PROP_FRAME_COUNT
getFrameNumber cap = unsafePerformIO $
                      {#call cvGetCaptureProperty#} 
                         cap cvCAP_POS_FRAMES

