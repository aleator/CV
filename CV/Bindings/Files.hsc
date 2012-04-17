{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Files where

import Foreign.Ptr
import Foreign.C.String
import CV.Bindings.Types
import CV.Image(BareImage)

#strict_import

#include <bindings.dsl.h>
#include "cvFiles.h"

#ccall read_from_tcr , CString ->  Ptr (Ptr CUInt) -> IO (Ptr BareImage)

#ccall read_from_tcr_rectified , CString -> IO (Ptr BareImage)

#ccall read_from_tcr_mag , CString -> IO (Ptr BareImage)

#ccall calc_derivative_direction , Ptr BareImage -> IO (Ptr BareImage)
