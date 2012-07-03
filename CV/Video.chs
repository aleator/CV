{-#LANGUAGE ForeignFunctionInterface, ViewPatterns, CPP#-}
#include "cvWrapLEO.h"
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

-- NOTE: For some reason, this module fails to work with ghci for me

{#pointer *CvCapture as Capture foreign newtype#}

foreign import ccall "& wrapReleaseCapture" releaseCapture :: FinalizerPtr Capture

{#pointer *CvVideoWriter as VideoWriter foreign newtype#}

foreign import ccall "& wrapReleaseVideoWriter" releaseVideoWriter :: FinalizerPtr VideoWriter
-- NOTE: This use of foreignPtr is quite likely to cause trouble by retaining
--       videos longer than necessary.

type VideoStream c d = Stream IO (Image c d)

streamFromVideo cap   = dropS 1 $ streamFromVideo' (undefined) cap 
streamFromVideo' p cap = Value $ do
                         x <- getFrame cap
                         case x of
                            Just f -> return (p,(streamFromVideo' f cap))
                            Nothing -> return (p,Terminated)
                        

captureFromFile fn = withCString fn $ \cfn -> do
                      ptr <- {#call cvCreateFileCapture#} cfn
                      fptr <- newForeignPtr releaseCapture ptr
                      return . Capture $ fptr

captureFromCam int = do
                      ptr <- {#call cvCreateCameraCapture#} (fromIntegral int)
                      if  ptr==nullPtr 
                        then 
                          return Nothing
                        else do
                          fptr <- newForeignPtr releaseCapture ptr
                          return . Just . Capture $ fptr

dropFrame cap = withCapture cap $ \ccap -> {#call cvGrabFrame#} ccap >> return ()

getFrame :: Capture -> IO (Maybe (Image RGB D32))
getFrame cap = withCapture cap $ \ccap -> do
                p_frame <- {#call cvQueryFrame#} ccap 
                if p_frame==nullPtr then return Nothing
                                    else creatingImage (ensure32F p_frame) >>= return . Just
                    -- NOTE: This works because Image module has generated wrappers for ensure32F

#c
enum CapProp {
      CAP_PROP_POS_MSEC       =  CV_CAP_PROP_POS_MSEC     
    , CAP_PROP_POS_FRAMES     =  CV_CAP_PROP_POS_FRAMES   
    , CAP_PROP_POS_AVI_RATIO  =  CV_CAP_PROP_POS_AVI_RATIO
    , CAP_PROP_FRAME_WIDTH    =  CV_CAP_PROP_FRAME_WIDTH  
    , CAP_PROP_FRAME_HEIGHT   =  CV_CAP_PROP_FRAME_HEIGHT 
    , CAP_PROP_FPS            =  CV_CAP_PROP_FPS          
    , CAP_PROP_FOURCC         =  CV_CAP_PROP_FOURCC       
    , CAP_PROP_FRAME_COUNT    =  CV_CAP_PROP_FRAME_COUNT  
    , CAP_PROP_FORMAT         =  CV_CAP_PROP_FORMAT       
    , CAP_PROP_MODE           =  CV_CAP_PROP_MODE         
    , CAP_PROP_BRIGHTNESS     =  CV_CAP_PROP_BRIGHTNESS   
    , CAP_PROP_CONTRAST       =  CV_CAP_PROP_CONTRAST     
    , CAP_PROP_SATURATION     =  CV_CAP_PROP_SATURATION   
    , CAP_PROP_HUE            =  CV_CAP_PROP_HUE          
    , CAP_PROP_GAIN           =  CV_CAP_PROP_GAIN         
    , CAP_PROP_EXPOSURE       =  CV_CAP_PROP_EXPOSURE     
    , CAP_PROP_CONVERT_RGB    =  CV_CAP_PROP_CONVERT_RGB  
#if defined OpenCV23 || defined OpenCV24
    , CAP_PROP_WHITE_BALANCE_BLUE_U = CV_CAP_PROP_WHITE_BALANCE_BLUE_U 
    , CAP_PROP_WHITE_BALANCE_RED_V  = CV_CAP_PROP_WHITE_BALANCE_RED_V
#else
    , CAP_PROP_WHITE_BALANCE  = CV_CAP_PROP_WHITE_BALANCE
#endif
    , CAP_PROP_RECTIFICATION  =  CV_CAP_PROP_RECTIFICATION 
    , CAP_PROP_MONOCROME      =  CV_CAP_PROP_MONOCROME    
};
#endc

{#enum CapProp {}#}

fromProp = fromIntegral . fromEnum

getCapProp cap prop = withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap (fromProp prop) >>= return . realToFrac

getFrameRate cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap (fromProp CAP_PROP_FPS) >>= return . realToFrac

getFrameSize cap = unsafePerformIO $
                      withCapture cap $ \ccap -> do
                         w <- {#call cvGetCaptureProperty#} ccap (fromProp CAP_PROP_FRAME_WIDTH) 
                                >>= return . round
                         h <- {#call cvGetCaptureProperty#} ccap (fromProp CAP_PROP_FRAME_HEIGHT)
                                >>= return . round
                         return (w,h)


setCapProp cap prop val = withCapture cap $ \ccap ->
                         {#call cvSetCaptureProperty#} 
                           ccap (fromProp prop) (realToFrac val)

numberOfFrames cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                           ccap (fromProp CAP_PROP_FRAME_COUNT)
                            >>= return . floor

frameNumber cap = unsafePerformIO $
                      withCapture cap $ \ccap ->
                         {#call cvGetCaptureProperty#} 
                          ccap (fromProp CAP_PROP_POS_FRAMES) >>= return . floor

-- Video Writing

data Codec = MPG4 deriving (Eq,Show)

createVideoWriter filename codec framerate frameSize = 
    withCString filename $ \cfilename -> do
        ptr <- {#call wrapCreateVideoWriter#} cfilename fourcc 
                                              framerate w h 0
        if ptr == nullPtr then error "Could not create video writer" else return ()
        fptr <- newForeignPtr releaseVideoWriter ptr
        return . VideoWriter $ fptr
  where
    (fromIntegral -> w, fromIntegral -> h) = frameSize
    fourcc | codec == MPG4 = 0x4d504734 -- This is so wrong..

writeFrame :: VideoWriter -> Image RGB D32 -> IO ()
writeFrame writer img = withVideoWriter writer $ \cwriter ->
                         withImage img    $ \cimg -> 
                          {#call cvWriteFrame #} cwriter cimg >> return ()
