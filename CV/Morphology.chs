{-#LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, UnicodeSyntax, ViewPatterns#-}
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
                  ,dilateOp,erodeOp,KernelShape(EllipseShape,CrossShape,RectShape) 
                  )
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
openOp :: StructuringElement -> ImageOperation GrayScale D32
openOp se = erodeOp se 1 #> dilateOp se 1                    
open se = unsafeOperate (openOp se) 
a ○ b = open b a
-- a ○ b = (a ⊖ b) ⊕ b 


-- Morphological closing
closeOp :: StructuringElement -> ImageOperation GrayScale D32
closeOp se = dilateOp se 1 #> erodeOp se 1                    
close se = unsafeOperate (closeOp se) 
a ● b = close b a

geodesic :: Image GrayScale D32 -> ImageOperation GrayScale D32 -> ImageOperation GrayScale D32
geodesic mask op = op #> IM.limitToOp mask

-- | Perform a black tophat filtering of size
blackTopHat size i =
                  let se = structuringElement 
                        (size,size) (size `div` 2, size `div` 2) RectShape
                      x  = unsafeOperate (closeOp se) i
                  in x `IM.sub` i

-- | Perform a white tophat filtering of size
whiteTopHat size i =
                  let se = structuringElement 
                        (size,size) (size `div` 2, size `div` 2) RectShape
                      x  = unsafeOperate (openOp se) i
                  in i `IM.sub` x

basicSE = structuringElement (3,3) (1,1) RectShape
bigSE = structuringElement (9,9) (4,4) RectShape

---------- Low level wrapper
#c
enum KernelShape {
    RectShape    = CV_SHAPE_RECT
    ,CrossShape   = CV_SHAPE_CROSS
    ,EllipseShape = CV_SHAPE_ELLIPSE
    ,CustomShape  = CV_SHAPE_CUSTOM
    };
#endc
{#enum KernelShape {} #}

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
             w h x y (fromIntegral . fromEnum $ shape) nullPtr
    fptr <- newForeignPtr releaseSE iptr
    return (ConvKernel fptr)

customSE s@(w,h) o shape | isGoodSE s o 
                         && length shape == fromIntegral (w*h)
                            = createCustomSE s o shape

createCustomSE (w,h) (x,y) shape = unsafePerformIO $ do
            iptr <- withArray shape $ \arr ->
                    {#call cvCreateStructuringElementEx#}
                      w h x y (fromIntegral . fromEnum $ CustomShape) arr
            fptr <- newForeignPtr releaseSE iptr
            return (ConvKernel fptr)

{#fun cvErode as erosion 
    {withGenBareImage* `BareImage'
    ,withGenBareImage* `BareImage'
    ,withConvKernel* `ConvKernel'
    ,`Int'} -> `()' #}
{#fun cvDilate as dilation 
    {withGenBareImage* `BareImage'
    ,withGenBareImage* `BareImage'
    ,withConvKernel* `ConvKernel'
    ,`Int'} -> `()' #}


erodeOp se count = ImgOp $ \(unS -> img)  -> erosion img img se count
dilateOp se count = ImgOp $ \(unS -> img) -> dilation img img se count

erode se count  i = unsafeOperate (erodeOp se count)  i
dilate se count i = unsafeOperate (dilateOp se count) i

a ⊕ b = erode b 1 a
a ⊖ b = erode b 1 a
                       
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
