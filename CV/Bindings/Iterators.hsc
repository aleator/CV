{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Iterators where

import Foreign.Ptr
import CV.Bindings.Types
import CV.Image(BareImage)

#strict_import

#include <bindings.dsl.h>
#include "cvIterators.h"

#starttype F32_image_iterator
#field image_data , Ptr Float
#stoptype

#ccall alloc_F32_image_iterator , IO (Ptr <F32_image_iterator>)
#ccall free_F32_image_iterator , Ptr <F32_image_iterator> -> IO ()
#ccall F32_create_rowwise_iterator , Ptr <F32_image_iterator> -> Ptr BareImage -> IO ()
#ccall F32_next , Ptr <F32_image_iterator> -> IO (Ptr <F32_image_iterator>)
#ccall F32_val , Ptr <F32_image_iterator> -> Ptr CFloat
#ccall F32_rowwise_pos , Ptr <F32_image_iterator> -> Ptr <CvPoint>
