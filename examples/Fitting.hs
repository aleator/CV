module Main where
import CV.Fitting

main = print $ fitEllipse [(0,0),(1,0),(0,1)]
