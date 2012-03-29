{-#LANGUAGE ParallelListComp, RecordWildCards, DeriveGeneric#-}
module Main where

import CV.Image
import CV.Tracking
import CV.Bindings.Types
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle
import Text.Printf
import CV.ImageMathOp hiding ((#>))
import CV.Filters
import CV.ColourUtils
import Utils.Rectangle 
import Data.List
import Data.Function

import Graphics.Tools.DefaultGUI

data Parameters = P {x :: IntRange Zero Hundred
                    ,y :: IntRange Zero Hundred
                    ,scale :: IntRange One Fifty } deriving (Generic)

instance Default Parameters
instance Persist Parameters
instance Tangible Parameters

main = defaultGUI "Treetop_params" "dsm.tif" f

f P{..} img = resultI
   where 
       prec :: Image GrayScale D32
       prec = gaussian (11,11) img 
       (w,h) = getSize img
       resultPts = [ (val,res) | x <- [0,15..w-10], y <- [0,15..h-10] 
                   , let (val,res) = hillClimber (indfun prec) (-1,(x,y))
                   ]
       groups = group2 (\a b -> abs (a-b) < value scale) (snd.snd) (fst.snd) $ resultPts
       resultI = img-- <## [ lineOp 0.8 1 s (res) #>
                    --       circleOp 2 (res) 1 Filled
                    --      | (s,res) <- resultPts
                    --      ]
                     <## [circleOp 0 m 10 (Stroked 1) | rs <- groups, let (v,m) = maximumBy (compare`on`fst) rs]

       -- (val,res) = -- meanShift prec s1 (EPS 1) 

group2 equal a b = concatMap (f a) . f b
 where
  f a = groupBy (equal `on` a) . sortBy (compare `on` a)


indfun :: Image GrayScale D32 -> ((Int,Int) -> D32)
indfun img c@(x,y) | x<0 = 0
                   | y<0 = 0
                   | x>=w = 0
                   | y>=h = 0
                   | otherwise = getPixel c img
                where (w,h) = getSize img

hillClimber :: ((Int,Int) -> D32) -> (D32, (Int,Int)) -> (D32, (Int,Int))
hillClimber f (o,init) 
    | (o+0.001) >= v = (o,init) 
    | otherwise = hillClimber f (v,next)    
    where
     (v,next) = maximumBy (compare`on` fst) $ [(f n, n) | n <- neighbours init ]
     neighbours (x,y) = [(x+i,y+j) | i<-[-5,-4..5], j <- [-5,-4..5], (i,j) /= (0,0)]  
