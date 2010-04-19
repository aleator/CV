module CV.Marking where

import CV.Image as Image
import CV.Morphology
import CV.Edges as Edges
import CV.ImageOp as ImageOp
import CV.Sampling
import qualified CV.ImageMath as IM
import CV.Drawing
import CV.ColourUtils
import Foreign.C.Types
import CV.ImageMathOp



-- For easy marking of detected flaws
boxFlaws i = Edges.laplace 1 $ dilate basicSE 5 (i)
highLightFlaws image flaws = displayFlaws 
                             ((0.2 |* flaws) #+ (0.8 |* image)) flaws
displayFlaws image = IM.sub image . IM.mulS 0.6 . boxFlaws 
displayLargeFlaws image = IM.sub image . IM.mulS 0.6 . Edges.laplace 1 


type Marker = (CInt,CInt) -> CDouble -> (CInt,CInt)
                -> ImageOperation

condMarker condition m size t place  = if condition t 
                                        then m size t place
                                        else nonOp

getCoordsForMarkedTiles tileSize overlap marks image = 
    map fst $ filter (snd) $ zip coords marks
 where
    coords = getOverlappedTileCoords tileSize overlap image

cuteDot (x,y) = 
        circleOp 
         (x,y) (w*2) 0.1 (Stroked 1) ImageOp.#> circleOp (x,y) (w*2-1) 0.9 (Stroked 1) 
    where w = 2 

cuteCircle1 (x,y) = 
        circleOp 
         (x+w,y+w) (w*2) 0.1 (Stroked 1) ImageOp.#> circleOp (x+w,y+w) (w*2-1) 0.9 (Stroked 1) 
    where w = 6 

cuteRect (w,h) (x,y) = 
        rectOp 0.1 1 (x,y) (x+w,y+h) ImageOp.#> 
        rectOp 1 1 (x+1,y+1) (x+w-1,y+h-1) 

cuteCircle ::  Marker
cuteCircle (tw,th) t (x,y) = 
                    (circleOp 
                     (x+tw`div`2,y+tw`div`2) (w) 0.1 (Stroked 1) ) ImageOp.#> circleOp (x+tw`div`2,y+tw`div`2) (w-1) 0.9 (Stroked 1) 
    where w = tw`div`2

markTiles image size overlap marker lst = marked
    where
        tileCoords = getOverlappedTileCoords size overlap image
        markers = map (\(t,c) -> marker size t c) $ zip lst tileCoords
        marked = unsafeOperate (foldl (ImageOp.#>) nonOp markers) image 
        
    
