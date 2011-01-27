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
import Utils.Stream

{#pointer *CvCapture as Capture foreign newtype#}

foreign import ccall "& wrapReleaseCapture" releaseCapture :: FinalizerPtr Capture

{#pointer *CvVideoWriter as VideoWriter foreign newtype#}

foreign import ccall "& wrapReleaseVideoWriter" releaseVideoWriter :: FinalizerPtr VideoWriter
-- NOTE: This use of foreignPtr is quite likely to cause trouble by retaining
--       videos longer than necessary.

type VideoStream c d = Stream IO (Image c d)

streamFromVideo cap   = dropS 1 $ streamFromVideo' undefined cap 
streamFromVideo' p cap = Value $ do
                         x <- getFrame cap
                         case x of
                            Just f -> return (p,(streamFromVideo' p cap))
                            Nothing -> return (p,Terminated)
                        

captureFromFile fn = withCString fn $ \cfn -> do
                      ptr <- {#call cvCreateFileCapture#} cfn
                      fptr <- newForeignPtr releaseCapture ptr
                      return . Capture $ fptr

captureFromCam int = do
                      ptr <- {#call cvCreateCameraCapture#} (fromIntegral int)
                      fptr <- newForeignPtr releaseCapture ptr
                      return . Capture $ fptr

dropFrame cap = withCapture cap $ \ccap -> {#call cvGrabFrame#} ccap >> return ()

getFrame :: Capture -> IO (Maybe (Image RGB D32))
getFrame cap = withCapture cap $ \ccap -> do
                p_frame <- {#call cvQueryFrame#} ccap 
                if p_frame==nullPtr then return Nothing
                                    else creatingImage (ensure32F p_frame) >>= return . Just
                    -- NOTE: This works because Image module has generated wrappers for ensure32F

-- These are likely to break..
cvCAP_PROP_POS_MSEC       =0 :: CInt
cvCAP_PROP_POS_FRAMES     =1 :: CInt
cvCAP_PROP_POS_AVI_RATIO  =2 :: CInt
cvCAP_PROP_FRAME_WIDTH    =3 :: CInt
cvCAP_PROP_FRAME_HEIGHT   =4 :: CInt
cvCAP_PROP_FPS            =5 :: CInt
cvCAP_PROP_FOURCC         =6 :: CInt
cvCAP_PROP_FRAME_COUNT    =7 :: CInt
cvCAP_PROP_FORMAT         =8 :: CInt
cvCAP_PROP_MODE           =9 :: CInt
cvCAP_PROP_BRIGHTNESS    =10 :: CInt
cvCAP_PROP_CONTRAST      =11 :: CInt
cvCAP_PROP_SATURATION    =12 :: CInt
cvCAP_PROP_HUE           =13 :: CInt
cvCAP_PROP_GAIN          =14 :: CInt
cvCAP_PROP_EXPOSURE      =15 :: CInt
cvCAP_PROP_CONVERT_RGB   =16 :: CInt
cvCAP_PROP_WHITE_BALANCE =17 :: CInt
cvCAP_PROP_RECTIFICATION =18 :: CInt   
cvCAP_PROP_MONOCROME     =19 :: CInt


getFrameRate cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap cvCAP_PROP_FPS >>= return . realToFrac

getFrameSize cap = unsafePerformIO $
                      withCapture cap $ \ccap -> do
                         w <- {#call cvGetCaptureProperty#} ccap cvCAP_PROP_FRAME_WIDTH >>= return . round
                         h <- {#call cvGetCaptureProperty#} ccap cvCAP_PROP_FRAME_HEIGHT >>= return . round
                         return (w,h)


setCapProp cap prop val = withCapture cap $ \ccap ->
                         {#call cvSetCaptureProperty#} 
                           ccap prop (realToFrac val)

getNumberOfFrames cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap cvCAP_PROP_FRAME_COUNT
                            >>= return . floor

getFrameNumber cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                          ccap cvCAP_PROP_POS_FRAMES >>= return . floor

-- Video Writing

data Codec = MPG4 deriving (Eq,Show)

createVideoWriter filename codec framerate frameSize isColor = 
    withCString filename $ \cfilename -> do
        ptr <- {#call wrapCreateVideoWriter#} cfilename fourcc 
                                              framerate w h ccolor
        if ptr == nullPtr then error "Could not create video writer" else return ()
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
