module CV.Projection where

import CV.Bindings.Projection
import CV.Image

import Foreign.Ptr
import System.IO.Unsafe

projectPolar :: Image c d -> Image GrayScale D32
projectPolar i = unsafePerformIO $ creatingImage $
  withImage i $ \i_ptr ->
    c'project_polar (castPtr i_ptr)
