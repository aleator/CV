{-#LANGUAGE RecordWildCards, ScopedTypeVariables, TypeFamilies#-}
module CV.Features (SURFParams, defaultSURFParams, getSURF
#ifndef OpenCV24
                   ,getMSER, MSERParams, mkMSERParams, defaultMSERParams
#endif
                   ,moments,Moments,getSpatialMoment,getCentralMoment,getNormalizedCentralMoment) where
import CV.Image
import CV.Bindings.Types
import CV.Bindings.Features
import Foreign.Ptr
import Control.Monad
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Utils.GeometryClass
import System.IO.Unsafe

#ifndef OpenCV24
newtype MSERParams = MP C'CvMSERParams deriving (Show)

-- | Create parameters for getMSER.
mkMSERParams :: Int            -- ^ Delta
                -> Int         -- ^ prune the area which bigger than maxArea
                -> Int         -- ^ prune the area which smaller than minArea
                -> Float       -- ^ prune the area have similar size to its children
                -> Float       -- ^ trace back to cut off mser with diversity < min_diversity
                -> Int         -- ^ for color image, the evolution steps
                -> Double      -- ^ the area threshold to cause re-initialize
                -> Double      -- ^ ignore too small margin
                -> Int         -- ^ the aperture size for edge blur
                -> MSERParams

mkMSERParams a b c d e f g h i= MP $ C'CvMSERParams a b c d e f g h i
defaultMSERParams = mkMSERParams 5 14400 60 0.25 0.2 200 1.01 0.003 5

-- | The function encapsulates all the parameters of the MSER extraction algorithm (see
--   <http://en.wikipedia.org/wiki/Maximally_stable_extremal_regions>
getMSER :: (Point2D a, ELP a~Int)
   => Image GrayScale D8 -> Maybe (Image GrayScale D8) -> MSERParams -> [[a]]
getMSER image mask (MP params) = unsafePerformIO $
   withMask mask $ \ptr_mask ->
   with nullPtr $ \ptr_ptr_contours ->
   withNewMemory $ \ptr_mem ->
   with params  $ \ptr_params ->
   withImage image $ \ptr_image -> do
    c'wrapExtractMSER (castPtr ptr_image) ptr_mask ptr_ptr_contours
                      ptr_mem ptr_params
    ptr_contours <- peek ptr_ptr_contours
    forM [0..10] $ \ix -> do
      ptr_ctr <- c'cvGetSeqElem ptr_contours ix
      ctr <- peek (castPtr ptr_ctr)
      pts :: [C'CvPoint] <- cvSeqToList ctr
      return (map convertPt pts)

#endif

-- TODO: Move this to some utility module
withMask :: Maybe (Image GrayScale D8) -> (Ptr C'CvArr -> IO α) -> IO α
withMask m f = case m of
               Just m  -> withImage m (f.castPtr)
               Nothing -> f nullPtr

-- | Parameters for SURF feature extraction
newtype SURFParams = SP C'CvSURFParams deriving Show
mkSURFParams :: Double
                    -- ^ only features with keypoint.hessian
                    -- larger than that are extracted.
                    -- good default value is ~300-500 (can depend on the
                    -- average local contrast and sharpness of the image).
                    -- user can further filter out some features based on
                    -- their hessian values and other characteristics.
                -> Int
                    -- ^ The number of octaves to be used for extraction.
                    -- With each next octave the feature size is doubled
                    -- (3 by default)
                -> Int
                    -- ^  The number of layers within each octave (4 by default)
                -> Bool
                    -- ^ If true, getSurf returns extended descriptors of 128 floats. Otherwise
                    --   returns 64 floats.
                -> SURFParams
mkSURFParams a b c d = SP $ C'CvSURFParams (fromBool d)
                                           (realToFrac a)
                                           (fromIntegral b)
                                           (fromIntegral c)


-- | Default parameters for getSURF
defaultSURFParams :: SURFParams
defaultSURFParams = mkSURFParams 400 3 4 False

-- | Extract Speeded Up Robust Features from an image.
getSURF :: SURFParams
            -- ^ Method parameters. See `defaultSURFParams` and `mkSURFParams`
            -> Image GrayScale D8
            -- ^ Input GrayScale image
            -> Maybe (Image GrayScale D8)
            -- ^ Optional Binary mask image
            -> [(C'CvSURFPoint,[Float])]
getSURF (SP params) image mask = unsafePerformIO $
   withNewMemory $ \ptr_mem ->
   withMask mask $ \ptr_mask ->
   with nullPtr $ \ptr_ptr_keypoints ->
   with nullPtr $ \ptr_ptr_descriptors ->
   with params  $ \ptr_params ->
   withImage image $ \ptr_image -> do
    ptr_keypoints' <- peek ptr_ptr_keypoints
    c'wrapExtractSURF (castPtr ptr_image) ptr_mask ptr_ptr_keypoints
                      ptr_ptr_descriptors ptr_mem ptr_params 0
    ptr_keypoints <- peek ptr_ptr_keypoints
    ptr_descriptors <- peek ptr_ptr_descriptors
    a <- cvSeqToList ptr_keypoints
    b <- if c'CvSURFParams'extended params /= 1
           then do
            es :: [FloatBlock128] <- cvSeqToList ptr_descriptors
            return (map (\(FP128 e) -> e) es)
           else do
            es :: [FloatBlock64] <- cvSeqToList ptr_descriptors
            return (map (\(FP64 e) -> e) es)
    return (zip a b)

newtype FloatBlock64  = FP64 [Float] deriving (Show)
newtype FloatBlock128 = FP128 [Float] deriving (Show)

instance Storable FloatBlock64 where
   sizeOf    _ = sizeOf (undefined :: Float) * 64
   alignment _ = 4
   peek ptr    = FP64 `fmap` peekArray 64 (castPtr ptr)
   poke ptr (FP64 e) = pokeArray (castPtr ptr) e

instance Storable FloatBlock128 where
   sizeOf    _ = sizeOf (undefined :: Float) * 128
   alignment _ = 4
   peek ptr    = FP128 `fmap` peekArray 128 (castPtr ptr)
   poke ptr (FP128 e) = pokeArray (castPtr ptr) e


type Moments = C'CvMoments


moments :: Image GrayScale D32 -> Moments
moments img = unsafePerformIO $
              withGenImage img $ \c_img ->
              with (C'CvMoments 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) $ \res -> do
               c'cvMoments c_img res 0
               peek res

getSpatialMoment :: (Int,Int) -> Moments -> Double
getSpatialMoment (x,y) m = realToFrac $
                     unsafePerformIO $
                     with m          $ \c_m ->
                      c'cvGetSpatialMoment c_m (fromIntegral x) (fromIntegral y)

getCentralMoment :: (Int,Int) -> Moments -> Double
getCentralMoment (x,y) m = realToFrac $
                     unsafePerformIO $
                     with m          $ \c_m ->
                      c'cvGetCentralMoment c_m (fromIntegral x) (fromIntegral y)

getNormalizedCentralMoment :: (Int,Int) -> Moments -> Double
getNormalizedCentralMoment (x,y) m = realToFrac $
                     unsafePerformIO $
                     with m          $ \c_m ->
                      c'cvGetNormalizedCentralMoment c_m (fromIntegral x) (fromIntegral y)
