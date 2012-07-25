{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified CV.Matrix as M
import CV.Transforms

main = do
  let mat = M.fromList (5,2) . concatMap (\(x,y) -> [x,y])
      source = [(1,1)
               ,(1,2)
               ,(2,1)
               ,(5,5)
               ,(2,4)]
      tr = map (\(x,y) -> (x+100,y+100)) source
      sc = map (\(x,y) -> (x*100,y*100)) source

  print (getHomography' (mat source) (mat $ tr ) Ransac 0.1)
  print (getHomography' (mat source) (mat $ sc ) Ransac 0.1)
  print (getHomography' (mat source) (mat source) LMeds 0.1)
