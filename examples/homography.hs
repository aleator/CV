{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified CV.Matrix as M
import CV.Transforms

main = do
  let source = M.fromList (5,2) [1,1
                                ,1,2
                                ,2,1
                                ,5,5
                                ,2,4]
      target = M.fromList (5,2) [1,1
                                ,1,2
                                ,2,1
                                ,5,5
                                ,1900,4]

  print (getHomography' source target Default 0.1)
  print (getHomography' source target Ransac 0.001)
  print (getHomography' source target LMeds 0.001)
