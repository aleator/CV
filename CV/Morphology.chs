{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables#-}
#include "cvWrapLEO.h"
module CV.Morphology (StructuringElement
                  ,structuringElement
                  ,customSE
                  ,basicSE,bigSE
                  ,geodesic
                  ,openOp,closeOp
                  ,open,close
                  ,erode,dilate
                  ,blackTopHat,whiteTopHat
                  ,dilateOp,erodeOp,ellipseShape
                  ,crossShape,rectShape) 
where

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array

import CV.Image 

import CV.ImageOp
import qualified CV.ImageMath as IM

import C2HSTools

-- Morphological opening
openOp :: StructuringElement -> ImageOperation
openOp se = erodeOp se 1 #> dilateOp se 1                    
open se = unsafeOperate (openOp se) 
-- Morphological closing
closeOp :: StructuringElement -> ImageOperation
closeOp se = dilateOp se 1 #> erodeOp se 1                    
close se = unsafeOperate (closeOp se) 

geodesic :: Image -> ImageOperation -> ImageOperation
geodesic mask op = op #> IM.limitToOp mask

blackTopHat size i = unsafePerformIO $ do
                  let se = structuringElement 
                        (size,size) (size `div` 2, size `div` 2) rectShape
                  x <- runImageOperation i (closeOp se)
                  return $ x `IM.sub` i

whiteTopHat size i = unsafePerformIO $ do
                  let se = structuringElement 
                        (size,size) (size `div` 2, size `div` 2) rectShape
                  x <- runImageOperation i (openOp se)
                  return $ i `IM.sub` x

basicSE = structuringElement (3,3) (1,1) rectShape
bigSE = structuringElement (9,9) (4,4) rectShape

---------- Low level wrapper
rectShape = 0
crossShape = 1
ellipseShape = 2
customShape = 100

{#pointer *IplConvKernel as ConvKernel foreign newtype#}

type StructuringElement = ConvKernel

foreign import ccall "& wrapReleaseStructuringElement" 
    releaseSE :: FinalizerPtr ConvKernel


-- Check morphology element
isGoodSE s@(w,h) d@(x,y) | x>=0 && y>=0 
                         && w>=0 && h>=0
                         && x<w  && y<h 
                         = True

                         | otherwise = False 


-- Create a structuring element for morphological operations
structuringElement s d | isGoodSE s d = createSE s d 
                       | otherwise = error "Bad values in structuring element"

-- Create SE with custom shape that is taken from flat list shape.
createSE (w,h) (x,y) shape = unsafePerformIO $ do
    iptr <- {#call cvCreateStructuringElementEx#}
             w h x y (fromIntegral shape) nullPtr
    fptr <- newForeignPtr releaseSE iptr
    return (ConvKernel fptr)

customSE s@(w,h) o shape | isGoodSE s o 
                         && length shape == fromIntegral (w*h)
                            = createCustomSE s o shape

createCustomSE (w,h) (x,y) shape = unsafePerformIO $ do
            iptr <- withArray shape $ \arr ->
                    {#call cvCreateStructuringElementEx#}
                      w h x y (fromIntegral customShape) arr
            fptr <- newForeignPtr releaseSE iptr
            return (ConvKernel fptr)

{#fun cvErode as erosion 
    {withGenImage* `Image'
    ,withGenImage* `Image'
    ,withConvKernel* `ConvKernel'
    ,`Int'} -> `()' #}
{#fun cvDilate as dilation 
    {withGenImage* `Image'
    ,withGenImage* `Image'
    ,withConvKernel* `ConvKernel'
    ,`Int'} -> `()' #}



erodeOp se count = ImgOp $ \img -> erosion img img se count
dilateOp se count = ImgOp $ \img -> dilation img img se count

erode se count  i = unsafeOperate (erodeOp se count)  i
dilate se count i = unsafeOperate (dilateOp se count) i

                       
erode' se count img = withImage img $ \image ->
               withConvKernel se $ \ck ->
             {#call cvErode#} (castPtr image) 
                              (castPtr image) 
                              ck count
                              
dilate' se count img = withImage img $ \image ->
               withConvKernel se $ \ck ->
             {#call cvDilate#} (castPtr image) 
                              (castPtr image) 
                              ck count
