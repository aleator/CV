module Utils.Pointer where 
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array

withPtrList :: [ForeignPtr a] -> (Ptr (Ptr a)-> IO b) -> IO b
withPtrList objs op = let
                    ptrs = map unsafeForeignPtrToPtr objs
                   in do
                    r <- withArray ptrs op
                    mapM_ touchForeignPtr objs 
                    return r

