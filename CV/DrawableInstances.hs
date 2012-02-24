{-#LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module CV.DrawableInstances where
import Utils.DrawingClass 
import Utils.GeometryClass
import CV.Drawing 
import CV.Image
import CV.ImageOp
import CV.Bindings.Types
import CV.Bindings.Features


instance Draws C'CvSURFPoint (ImageOperation GrayScale D32) where
    draw (C'CvSURFPoint (C'CvPoint2D32f x y) l s d h) 
     = circleOp 1 (round x, round y) (fromIntegral s) (Stroked 1) 
        #> lineOp 1 1 (round x,round y) (round $ x+fromIntegral s*cos (realToFrac d)
                                        ,round $ y+fromIntegral s*sin (realToFrac d))  
    
instance Draws C'CvPoint2D32f (ImageOperation GrayScale D32) where
    draw (C'CvPoint2D32f x y) = circleOp 1 (round x, round y) 3 Filled
    

