module CV.Fitting where

import CV.Bindings.Fittings
import CV.Bindings.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe
import CV.Matrix

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
           return (Ellipse (x,y)
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
