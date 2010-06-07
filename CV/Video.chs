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

{#pointer *CvVideoWriter as VideoWriter foreign newtype#}

foreign import ccall "& wrapReleaseVideoWriter" releaseVideoWriter :: FinalizerPtr VideoWriter
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

cvCAP_PROP_FRAME_COUNT = 7 -- These are likely to break..
cvCAP_PROP_FPS = 5
cvCAP_PROP_FRAME_WIDTH = 3
cvCAP_PROP_FRAME_HEIGHT = 4
cvCAP_POS_FRAMES = 1

getFrameRate cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap cvCAP_PROP_FPS >>= return . realToFrac

getFrameSize cap = unsafePerformIO $
                      withCapture cap $ \ccap -> do
                         w <- {#call cvGetCaptureProperty#} ccap cvCAP_PROP_FRAME_WIDTH >>= return . round
                         h <- {#call cvGetCaptureProperty#} ccap cvCAP_PROP_FRAME_HEIGHT >>= return . round
                         return (w,h)


getNumberOfFrames cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap cvCAP_PROP_FRAME_COUNT
                            >>= return . floor

getFrameNumber cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                          ccap cvCAP_POS_FRAMES >>= return . floor

-- Video Writing

data Codec = MPG4 deriving (Eq,Show)

createVideoWriter filename codec framerate frameSize isColor = 
    withCString filename $ \cfilename -> do
        ptr <- {#call wrapCreateVideoWriter#} cfilename fourcc 
                                              framerate w h ccolor
        fptr <- newForeignPtr releaseVideoWriter ptr
        return . VideoWriter $ fptr
  where
    (w,h) = frameSize
    ccolor | isColor   = 1
           | otherwise = 0
    fourcc | codec == MPG4 = 0x4d504734 -- This is so wrong..

writeFrame writer img = withVideoWriter writer $ \cwriter ->
                         withImage img    $ \cimg -> 
                          {#call cvWriteFrame #} cwriter cimg
