module CV.ImageOp where

import Foreign
import CV.Image

-- |ImageOperation is a device for mutating images inplace.
newtype ImageOperation c d= ImgOp (Image c d-> IO ())

-- |Compose two image operations
(#>) :: ImageOperation c d-> ImageOperation c d -> ImageOperation c d
(#>) (ImgOp a) (ImgOp b) = ImgOp (\img -> (a img >> b img))

-- |An unit operation for compose (#>) 
nonOp = ImgOp (\i -> return ())

-- |Apply image operation to a Copy of an image
img <# op = unsafeOperate op img

-- |Apply list of image operations to a Copy of an image. (Makes a single copy and is
-- faster than folding over (<#)
img <## [] = img
img <## op = unsafeOperate (foldl1 (#>) op) img

-- |Iterate an operation N times
times n op = foldl (#>) nonOp (replicate n op) 

-- This could, if I take enough care, be pure.
runImageOperation :: Image c d -> ImageOperation c d -> IO (Image c d)
runImageOperation img (ImgOp op) = withClone img $ \clone -> 
                                    op clone >> return clone

directOp i (ImgOp op)  = op i
operateInPlace (ImgOp op) img = op img 

operate op img = runImageOperation img op
operateOn = runImageOperation
unsafeOperate op img = unsafePerformIO $ operate op img
unsafeOperateOn img op = unsafePerformIO $ operate op img

operateWithROI pos size (ImgOp op) img = withClone img $ \clone ->
                                  withIOROI pos size clone (op clone)
