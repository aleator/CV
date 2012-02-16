{-#LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies#-}
module CV.Tracking where
import CV.Bindings.Tracking
import CV.Bindings.Types
import CV.Image
import Utils.GeometryClass
import Utils.Rectangle
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe

meanShift :: (IntBounded a, ELBB a~Int) => Image GrayScale D32 -> a -> TermCriteria
               -> (Double,Rectangle Int)
meanShift image window crit = unsafePerformIO $
   withGenImage image $ \c_img    ->
   with (convertBounds window) $ \c_window ->
   with (toCvTCrit crit) $ \c_crit   ->
   with (C'CvConnectedComp 0 (C'CvScalar 0 0 0 0) (C'CvRect 0 0 0 0) nullPtr) $ \c_conn -> do
    c'wrapMeanShift c_img c_window c_crit c_conn
    C'CvConnectedComp area _ rect _ <- peek c_conn
    return (realToFrac area, bounds rect)

snake image initialPts α β γ (w,h) termcrit gradient = do
   with α $ \c_α ->
    with β $ \c_β ->
     with γ $ \c_γ ->
      with (C'CvSize w h) $ \ c_win ->
       with termcrit $ \ c_crit ->
        withArray (map toPt initialPts) $ \c_pts ->
         withImage image $ \c_image -> do
          c'cvSnakeImage (castPtr c_image)
                           c_pts
                           (fromIntegral (length initialPts))
                           c_α c_β c_γ
                           c'CV_VALUE
                           c_win
                           c_crit
                           (fromBool gradient)
          peekArray (length initialPts) c_pts

