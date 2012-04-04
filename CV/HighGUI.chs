{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.HighGUI where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import CV.Image
{#import CV.Image#}
import CV.ImageOp

-- Functions for easy operation

-- TODO: "__TMP__" should be a gensym
display image = do
        makeWindow "__TMP__"
        showImage "__TMP__" image
        --threadDelay 2000000
        waitKey 0
        destroyWindow "__TMP__"

--- Lower level interface
{#fun cvNamedWindow as mkWin {withCString* `String', `Int' } -> `()' #}

makeWindow name = mkWin name 1

destroyWindow n = withCString n $ \name -> do
                {#call cvDestroyWindow#} name

foreign import ccall "wrapper"
  trackbarCallback :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))

mkTrackbar mx initial name window callback = do
        cb <- trackbarCallback callback
        withCString name $ \cname ->
         withCString window $ \cwindow ->
          {#call cvCreateTrackbar#} cname cwindow nullPtr (fromIntegral mx) cb
    
waitKey delay = {#call cvWaitKey#} delay

{#fun cvShowImage as showImage
 {`String', withGenImage* `Image c d'} -> `()'#}
