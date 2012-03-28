{-#LANGUAGE FlexibleContexts#-}
-- | Mathematical operators for images; see also module "ImageMath" for the
--   functions these operators are based on.
module CV.ImageMathOp(
  -- * Operators for two images
  (#+)
, (#-)
, (#*)
, (#<)
, (#>)
  -- * Operators for an image and a scalar
, (|*)
, (|+)
, (-|)
, (|-)
, (|>)
, (|<)
) where

import CV.Image
import CV.ImageMath as IM
import Data.List(iterate)

-- | Image addition, subtraction, and multiplication operator; same as 
--   'ImageMath.add', 'ImageMath.sub', and 'ImageMath.mul'.
(#+), (#-), (#*) :: (CreateImage (Image c d)) => Image c d -> Image c d -> Image c d

(#+) = IM.add
(#-) = IM.sub
(#*) = IM.mul

-- | Image comparison operators; same as 'ImageMath.less2Than' and
--   'ImageMath.more2Than'. Example: @A #< B@ produces a binary image that has 
--   white pixels in those positions where value of A is less than value of B.
(#<), (#>) :: (CreateImage (Image GrayScale d)) => Image GrayScale d -> Image GrayScale d 
            -> Image GrayScale D8
            
(#<) = IM.less2Than
(#>) = IM.more2Than

-- | Scalar multiplication, addition, and subtraction (scalar on left) operators;
--   same as 'ImageMath.mulS', 'ImageMath.addS', and 'ImageMath.subRS'.
(|*), (|+), (-|) ::  D32 -> Image GrayScale D32 -> Image GrayScale D32

(|*) = IM.mulS
(|+) = IM.addS
(-|) = IM.subRS

-- | Scalar comparison operators; same as 'ImageMath.moreThan' and
--   'ImageMath.lessThan'. Example: @s |> I@ produces a binary image that has
--   white pixels in those positions where the value of I is larger than s.
--   Notice that this is opposite to the intuitive interpretation.
(|>), (|<) ::  D32 -> Image GrayScale D32 -> Image GrayScale D8

(|>) = IM.moreThan
(|<) = IM.lessThan

-- (|^) i n = (iterate (#* i) i) !! (n-1)

-- | Scalar subtraction operator (scalar on right); same as 'ImageMath.subS'.
(|-)  :: Image GrayScale D32 -> D32 -> Image GrayScale D32
(|-) = IM.subS
