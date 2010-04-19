module CV.ImageMathOp where
import CV.Image
import CV.ImageMath as IM
import Data.List(iterate)

(#+) = IM.add
(#-) = IM.sub
(#*) = IM.mul
(|*) = IM.mulS
(|+) = IM.addS
(|-) = IM.subS
(-|) = IM.subRS
(#<) = IM.less2Than
(#>) = IM.more2Than
(|>) = IM.moreThan
(|<) = IM.lessThan
(|^) i n = (iterate (#* i) i) !! (n-1)
