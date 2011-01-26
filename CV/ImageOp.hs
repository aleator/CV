module CV.ImageOp where

import Foreign
import CV.Image

-- Testing how to handle operation sequences without 
-- copying image for each operation.
newtype ImageOperation c d= ImgOp (Image c d-> IO ())
(#>) :: ImageOperation c d-> ImageOperation c d -> ImageOperation c d
(#>) (ImgOp a) (ImgOp b) = ImgOp (\img -> (a img >> b img))
nonOp = ImgOp (\i -> return ())

img <# op = unsafeOperate op img
img <## [] = img
img <## op = unsafeOperate (foldl1 (#>) op) img

times n op = foldl (#>) nonOp (replicate n op) 

-- This could, if I take enough care, be pure.
runImageOperation :: Image c d -> ImageOperation c d -> IO (Image c d)
runImageOperation img (ImgOp op) = withClone img $ \clone -> 
                                    op clone >> return clone

directOp i (ImgOp op)  = op i
operate op img = runImageOperation img op
operateOn = runImageOperation
unsafeOperate op img = unsafePerformIO $ operate op img
unsafeOperateOn img op = unsafePerformIO $ operate op img

operateWithROI pos size (ImgOp op) img = withClone img $ \clone ->
                                  withIOROI pos size clone (op clone)
