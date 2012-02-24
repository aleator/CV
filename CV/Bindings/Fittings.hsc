{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Fittings where
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import CV.Bindings.Types

#include <bindings.dsl.h>
#include "cvWrapLEO.h"

#ccall wrapFitEllipse , Ptr <CvArr> -> Ptr <CvBox2D> -> IO ()
#ccall cvFitLine      , Ptr <CvArr> -> CInt -> Double  -> Double -> Double-> Ptr Float -> IO ()
#ccall wrapMinAreaRect2 , Ptr <CvArr> -> Ptr <CvMemStorage> -> Ptr <CvBox2D> -> IO ()

#num CV_DIST_USER    
#num CV_DIST_L1      
#num CV_DIST_L2      
#num CV_DIST_C       
#num CV_DIST_L12     
#num CV_DIST_FAIR    
#num CV_DIST_WELSCH  
#num CV_DIST_HUBER   

data Dist = 
        Dist_User    
      | Dist_L1      
      | Dist_L2      
      | Dist_C       
      | Dist_L12     
      | Dist_Fair    
      | Dist_Welsch  
      | Dist_Huber   

toNum Dist_User   =  c'CV_DIST_USER  
toNum Dist_L1     =  c'CV_DIST_L1    
toNum Dist_L2     =  c'CV_DIST_L2    
toNum Dist_C      =  c'CV_DIST_C     
toNum Dist_L12    =  c'CV_DIST_L12   
toNum Dist_Fair   =  c'CV_DIST_FAIR  
toNum Dist_Welsch =  c'CV_DIST_WELSCH
toNum Dist_Huber  =  c'CV_DIST_HUBER 

