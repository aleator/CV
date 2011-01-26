module CV.ImageMathOp where
import CV.Image
import CV.ImageMath as IM
import Data.List(iterate)

(#+), (#-), (#*), (#<), (#>) :: (CreateImage (Image c d)) => Image c d -> Image c d -> Image c d
(#+) = IM.add
(#-) = IM.sub
(#*) = IM.mul
(#<) = IM.less2Than
(#>) = IM.more2Than

(|*), (|+), (-|), (|>), (|<) ::  D32 -> Image GrayScale D32 -> Image GrayScale D32
(|*) = IM.mulS
(|+) = IM.addS
(-|) = IM.subRS
(|>) = IM.moreThan
(|<) = IM.lessThan
-- (|^) i n = (iterate (#* i) i) !! (n-1)

(|-)  :: Image GrayScale D32 -> D32 -> Image GrayScale D32
(|-) = IM.subS
