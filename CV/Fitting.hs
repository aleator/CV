{-#LANGUAGE TypeFamilies#-}
module CV.Fitting where

import CV.Bindings.Fittings
import CV.Bindings.Types
import CV.Matrix
import CV.Image(getSize)
import Control.Applicative
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Utils.GeometryClass

data Ellipse = Ellipse {center::(Float,Float)
                       ,width,height::Float
                       ,angle :: Float} deriving (Eq,Show)

-- | Given a (1,n) or (n,1) matrix of points, calculate
-- (in the least squares sense) the best ellipse around the
-- points
fitEllipse :: Matrix (Float,Float) -> Ellipse
fitEllipse pts = unsafePerformIO $
   withMatPtr pts $ \cMat ->
   alloca $ \result -> do
           c'wrapFitEllipse
            (castPtr cMat)
            result
           C'CvBox2D (C'CvPoint2D32f x y)
                     (C'CvSize2D32f w h)
                     a <- peek result
           return (Ellipse (realToFrac x,realToFrac y)
                           (realToFrac w)
                           (realToFrac h)
                           (realToFrac a))

-- | Fit a line to set of points.
fitLine2D :: Dist -> Double -> Double -> Double
             -> Matrix (Float,Float)
             -> ((Float,Float),(Float,Float))
fitLine2D dist_type param reps aeps pts = unsafePerformIO $
   withMatPtr pts $ \cMat ->
   allocaArray 4 $ \result -> do
           c'cvFitLine
            (castPtr cMat)
            (toNum dist_type) param reps aeps result
           [x,y,dx,dy] <- peekArray 4 result
           return ((x,y),(dx,dy))

-- | Fit a minimum area rectangle over a set of points
minAreaRect :: Matrix (Float,Float) -> C'CvBox2D
minAreaRect pts = unsafePerformIO $ 
   withMatPtr pts $ \cMat ->
   with (C'CvBox2D (C'CvPoint2D32f 0 0) (C'CvSize2D32f 0 0) 0) $ \result -> do
           c'wrapMinAreaRect2 (castPtr cMat) nullPtr result
           peek result

-- | Calculate the minimum axis-aligned bounding rectangle of given points.
boundingRect :: Matrix (Float,Float) -> C'CvRect
boundingRect pts = unsafePerformIO $ 
   withMatPtr pts $ \cMat ->
   with (C'CvRect 0 0 0 0) $ \result -> do
           c'wrapBoundingRect (castPtr cMat) 0 result
           peek result

-- | Calculate the minimum enclosing circle of a point set.
boundingCircle :: (ELP a ~ Double, Point2D a) => Matrix (Float,Float) -> (a, Double)
boundingCircle pts = unsafePerformIO $ 
   withMatPtr pts $ \cMat ->
   with 0         $ \cRadius ->   
   with (C'CvPoint2D32f 0 0 ) $ \result -> do
           c'cvMinEnclosingCircle (castPtr cMat) result cRadius
           (,) <$> (convertPt <$> peek result) <*> (realToFrac <$> peek cRadius)

-- | Calculcate the clockwise convex hull of a point set
convexHull :: Matrix (Float,Float) -> Matrix (Float,Float)
convexHull pts =  
 let res = create (getSize pts) :: Matrix (Float,Float)
 in  unsafePerformIO $
     withMatPtr pts $ \cMat ->
     withMatPtr res $ \cRes ->
     withNewMemory  $ \ptr_mem -> do
     with (C'CvPoint2D32f 0 0 ) $ \result -> do
             c'cvConvexHull2 (castPtr cMat) (castPtr cRes) c'CV_CLOCKWISE 1
             return res

