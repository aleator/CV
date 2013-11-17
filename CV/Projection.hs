module CV.Projection where

import CV.Bindings.Projection
import CV.Image
import CV.Matrix

import Foreign.Ptr
import System.IO.Unsafe

projectPolar :: Image c d -> Image GrayScale D32
projectPolar i = unsafePerformIO $ creatingImage $
  withImage i $ \i_ptr ->
    c'project_polar (castPtr i_ptr)

computeEpilines :: Matrix (Float,Float) -- ^ The set of points
                   -> Int               -- ^ Which camera (either 1 or 2)
                   -> Matrix Float      -- ^ Fundamental matrix
                   -> Matrix (Float,Float,Float)      -- ^ Output epilines
computeEpilines points cam fund = unsafePerformIO $ do
    res <- CV.Matrix.create (n,1) 
    withMatPtr points $ \c_points ->
     withMatPtr fund  $ \c_fund   ->
     withMatPtr res   $ \c_res    -> 
     c'cvComputeCorrespondEpilines c_points (fromIntegral cam) c_fund c_res
    return res
  where n = case getSize points of
                (1,n') -> n'
                (n',1) -> n'
                _      -> error "points should be 1 x N or N x 1 matrix" 

projectPoints :: Matrix (Float,Float,Float)  -- ^ object points
                -> Matrix Float              -- ^ rotation vector
                -> Matrix Float              -- ^ translation vector
                -> Matrix Float              -- ^ camera matrix
                -> Matrix Float              -- ^ distortion coeffs (4,5, or 8 elements)
                -> Matrix (Float, Float)     -- ^ image_points
projectPoints obj rot tra cam dist = unsafePerformIO $ do
    im <- CV.Matrix.create (getSize obj)
    withMatPtr obj $ \c_obj ->
     withMatPtr rot $ \c_rot ->
     withMatPtr tra $ \c_tra ->
     withMatPtr cam $ \c_cam ->
     withMatPtr dist $ \c_dist ->
     withMatPtr im $ \c_im -> do
         c'cvProjectPoints2 c_obj c_rot c_tra c_cam c_dist c_im 
                            nullPtr nullPtr
                            nullPtr nullPtr
                            nullPtr 0
         return im

triangulatePoints :: Matrix Float  -- ^ 3x4 projection
                   -> Matrix Float -- ^ 3x4 projection
                   -> Matrix Float -- ^ 2xN points
                   -> Matrix Float -- ^ 2xN points
                   -> Matrix Float -- ^ 4xN reconstructedPoints
triangulatePoints proj1 proj2 pts1 pts2 = unsafePerformIO $ do 
    res <- CV.Matrix.create (4, snd $ getSize pts1)
    withMatPtr proj1 $ \c_proj1 ->
     withMatPtr proj2 $ \c_proj2 ->
     withMatPtr pts1 $ \c_pts1 ->
     withMatPtr pts2 $ \c_pts2 ->
     withMatPtr res $ \c_res -> do
         c'cvTriangulatePoints c_proj1 c_proj2 c_pts1 c_pts2 c_res
         return res
    
