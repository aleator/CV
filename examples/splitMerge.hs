module Main where
import CV.Image
import CV.Sampling

-- | Output:
--      splitLena.jpg  - Lena image split to tiles with few pixels of black between tiles
--      splitLena2.jpg - Lena image split to tiles and joined safely back to original
main = do
    Just x <- loadImage "smallLena.jpg"
    let pieces = getTiles (30,30) x
        piecesWithCoordinates = getTilesC (30,30) x
        joined = montage (6,6) 5 pieces
        blitted = blitM (205,205) piecesWithCoordinates
    saveImage "splitLena.jpg" joined 
    saveImage "splitLena2.jpg" blitted

