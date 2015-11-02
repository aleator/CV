{-# LANGUAGE FlexibleContexts #-}
module CV.ImageOp where

import System.IO.Unsafe (unsafePerformIO)
import CV.Image
import Control.Monad ((>=>))
import Data.Monoid
import Control.Category
import Prelude hiding ((.),id)

-- |ImageOperation is a name for unary operators that mutate images inplace.
newtype ImageOperation c d = ImgOp (Image c d-> IO ())

-- |Compose two image operations
(#>) :: ImageOperation c d-> ImageOperation c d -> ImageOperation c d
(#>) (ImgOp a) (ImgOp b) = ImgOp (\img -> (a img >> b img))

-- |An unit operation for compose
nonOp = ImgOp (\i -> return ())

-- |Apply image operation to a Copy of an image
img <# op = unsafeOperate op img

blitOp img pos = ImgOp $ \i -> blit img i pos

setPixelOp pos v = ImgOp $ \i -> setPixel pos v i

-- motivating example:
-- >>> hop i = stretchHistogram $ i #- gaussian (5,5)
-- allocates two extra images
-- >>> hop = (gaussian (5,5) &#& id) #> subtract #> stretchHistogram
-- could be implemented in a way that allocates just one extra
fromImageOp (ImgOp f) = IOP $ \i -> (f i >> return i)

newtype IOP a b = IOP (a -> IO b)

instance Category IOP where
    id = IOP return
    (IOP f) . (IOP g)  = IOP $ g >=> f

(&#&) :: IOP (Image c d) e -> IOP (Image c d) f -> IOP (Image c d) (Image c d,Image c d)
(IOP f) &#& (IOP g) = IOP $ op
    where
        op i = withCloneValue i $ \cl -> (f i >> g cl >> return (i,cl))

unsafeOperate op img = unsafePerformIO $ operate op img

runIOP (IOP f) img = withMutableClone img $ \clone -> f clone

-- |Apply list of image operations to a Copy of an image. (Makes a single copy and is
-- faster than folding over (<#)
img <## [] = img
img <## op = unsafeOperate (foldl1 (#>) op) img


operate ::ImageOperation c d -> Image c d -> IO (Image c d)
operate (ImgOp op) img = withCloneValue img $ \clone ->
                                    op clone >> return clone

operateOn = flip operate

-- |Iterate an operation N times
times n op = foldl (#>) nonOp (replicate n op)

directOp i (ImgOp op)  = op i
operateInPlace (ImgOp op) img = op img

unsafeOperateOn img op = unsafePerformIO $ operate op img

--operateWithROI pos size (ImgOp op) img = withMutableClone img $ \clone ->
--                                            withIOROI pos size clone (op clone)
