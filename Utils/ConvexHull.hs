module Utils.ConvexHull
    ( convexHull
    ) where

import Data.List
import Data.Ord

convexHull :: [(Double,Double)] -> [(Double,Double)]
convexHull lst =
    let frst = minPoint lst
    in case frst of
        Nothing -> []
        Just f ->
            let sorted = sortBy (comparing (heading f)) lst
            in case sorted of
                   (a:b:cs) -> grahamScan (b:a:f:[]) cs
                   cs       -> f : cs
  where
    grahamScan [] _  = []
    grahamScan ps [] = ps
    grahamScan (x:[]) _ = [x]
    grahamScan (p2:p1:ps) (x:xs) =
        case turn p1 p2 x of
            LeftTurn -> grahamScan (x:p2:p1:ps) xs
            Straight -> grahamScan (x:p2:p1:ps) xs
            _        -> grahamScan (p1:ps) (x:xs)

heading :: (Double,Double) -> (Double,Double) -> Double
heading (x1,y1) (x2,y2) = atan2 (y2-y1) (x2-x1)

minPoint :: [(Double,Double)] -> Maybe (Double,Double)
minPoint [] = Nothing
minPoint xs = Just $ minimumBy (comparing snd) xs

data Turn = LeftTurn | RightTurn | Straight deriving (Eq, Ord, Show, Read)

turn :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Turn
turn a b c =
    let h1 = heading a b
        h2 = heading b c
        d = h2 - h1
    in if d >= 0 && d < pi then LeftTurn else RightTurn
