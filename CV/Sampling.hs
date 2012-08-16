module CV.Sampling where

import CV.Image
import Control.Monad.Primitive
import Control.Monad
import System.Random.MWC

import Foreign.C.Types
import qualified CV.ImageMath as IM
import Data.List(partition)

-- Get a patch around every pixel of given size for which it is 
-- attainable (Enough far from edge)
allPatches size image = [getRegion (x,y) size image 
                        | x <- [0..w-1], y <- [0..h-1]]
                    where
                     (wi,hi) = getSize image
                     (wp,hp) = size
                     (w,h) = (wi-wp,hi-hp)

allButLast = reverse.tail.reverse 
-- Get all non-overlapping patches of image
getTiles size image = getOverlappedTiles size (0,0) image
getTilesC size image = getOverlappedTilesC size (0,0) image

-- Get Coordinates for overlapping tiles
getOverlappedTileCoords size (xover,yover) image 
                    = [(x,y)
                      | x <- [0,wstep..wi-w-1]
                      , y <- [0,hstep..hi-h-1]]
                    where
                     (w,h) = size
                     (wi,hi) = getSize image
                     (wstep,hstep) = (floor $ fromIntegral w*(1-xover)
                                     ,floor $ fromIntegral h*(1-yover))

-- Get overlapping tiles
getOverlappedTiles s o i = map snd $ getOverlappedTilesC s o i  
getOverlappedTilesC :: (Int,Int) -> (CDouble,CDouble) -> Image c d -> [((Int,Int),Image c d)]
getOverlappedTilesC size overlap image 
                    = map (\c -> (both fromIntegral c,getRegion c size image))
                            $ getOverlappedTileCoords size 
                                overlap image
both f (a,b) = (f a, f b)
                    
getMarkedAndUnmarkedTiles size overlap image marks = 
    (map fst markedTiles,map fst nonMarked)
    where
        samples = getOverlappedTiles size overlap image
        marked = getOverlappedTiles size overlap marks
        ismarked (_,m) = IM.maxValue m > 0.9
        (markedTiles,nonMarked)  = partition ismarked 
                                    $ zip samples marked
 

-- get patches of image at `coords`
getPatches size coords image = map (\c -> getRegion c size image) coords

getCenteredPatches size coords image = map (\c -> getRegion (adjust c) 
                                                  size image) 
                                            coords
                                        where
                                         (w,h) = size
                                         adjust (x,y) = (x-w`div`2
                                                        ,y-h`div`2)

---- Make a random selections in IO monad
--randomSelect lst = randomRIO (0,length lst -1) >>= \x ->
--                              return (lst !! x)
                              
-- select k lst = sequence $ replicate k (randomSelect lst)

-- Discard coords around image borders. Useful for safely picking patches
discardAroundEdges (iw,ih) (vb,hb) coords = filter inRange coords
    where 
     inRange (x,y) =  vb<x && x< iw-vb
                   && hb<y && y< ih-hb


-- Retrive coordinates of white pixels (>0.9, arbitarily) of
-- image `marks`
getCoordsFromMarks marks = [(x,y) | x <- [0..w-1]
                                  , y <- [0..h-1]
                                  , getPixel (x,y) marks >0.9]
                        where (w,h) = getSize marks

getMarkedPatches size source marks 
    | getSize source == getSize marks = getPatches size coords source
    | otherwise = error "Image sizes mismatch"                            
    where coords = getCoordsFromMarks marks


-- Get some random image patches
randomPatches size count image gen = do
    coords <- replicateM count $ randomCoord (w,h) gen
    return $ getPatches size coords image
 where
    (pwidth,pheight) = size
    (iwidth,iheight) = getSize image
    (w,h) = (iwidth - pwidth , iheight-pheight) 

-- Get some random pixels from image
-- randomPixels count image = do
--    coords <- replicateM count $ randomCoord size
--    return $ map (flip getPixel $ image) $ coords 
--  where
--   size = getSize image

---- Get some random coords from image
--randomCoords :: MonadRandom m => Int -> (Int,Int) -> m [(Int,Int)]
--randomCoords count area = replicateM count $ randomCoord area

randomCoord :: PrimMonad m => (Int,Int) -> Gen (PrimState m) -> m (Int,Int)
randomCoord (w,h) g = do
            x <- uniformR (0::Int,fromIntegral $ w-1) g
            y <- uniformR (0::Int,fromIntegral $ h-1) g
            return (x,y) 
