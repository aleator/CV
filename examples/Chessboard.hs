module Main where
import CV.Image
import CV.Calibration

main = do
    Just i <- loadColorImage "chess.png"
    let corners = findChessboardCorners (unsafeImageTo8Bit i) (4,5) (FastCheck:defaultFlags)
    let y = drawChessboardCorners (unsafeImageTo8Bit i) (4,5) corners
    mapM_ print (corners)
    saveImage "found_chessboard.png" y
