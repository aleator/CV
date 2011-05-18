-- |Binary instances for images. Currently it only supports the type 
-- `Image Grayscale D32`.
{-#LANGUAGE ScopedTypeVariables, FlexibleInstances#-}
module CV.Binary where
import CV.Image (Image,GrayScale,D32)
import CV.Conversions

import Data.Maybe (fromJust)
import Data.Binary
import Data.Array.CArray
import Data.Array.IArray


-- NOTE: This binary instance is NOT PORTABLE.  

instance Binary (Image GrayScale D32) where
    put img = do
            let arr :: CArray (Int,Int) Double = copyImageToCArray img
            put (bounds arr)
            put . unsafeCArrayToByteString $ arr
    get = do 
            bds <- get
            get >>= return . copyCArrayToImage . fromJust . unsafeByteStringToCArray bds    

