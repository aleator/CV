{-#LANGUAGE RecordWildCards, ScopedTypeVariables#-}
module CV.Features (SURFParams, defaultParams, getSURF) where
import CV.Image
import CV.Bindings.Types
import CV.Bindings.Features
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

-- | Parameters for SURF feature extraction
data SURFParams = SURF {hessianThreshold :: Double
                            -- ^  Quoting OpenCV documentation ''only features with keypoint.hessian
                            -- larger than that are extracted.
                            -- good default value is ~300-500 (can depend on the
                            -- average local contrast and sharpness of the image).
                            -- user can further filter out some features based on
                            -- their hessian values and other characteristics.''
                       , nOctaves
                            -- ^ '' the number of octaves to be used for extraction.
                            -- With each next octave the feature size is doubled
                            -- (3 by default) ''
                       , nOctaveLayers :: Int
                            -- ^ '' The number of layers within each octave (4 by default) ''
                       , extended :: Bool
                            -- ^ If true, getSurf returns extended descriptors of 128 floats. Otherwise
                            --   returns 64 floats.
                        }

-- | Default parameters for getSURF
defaultParams :: SURFParams
defaultParams = SURF 400 3 4 False

-- | Extract Speeded Up Robust Features from an image
getSURF :: SURFParams -> Image GrayScale D8 -> IO [(C'CvSURFPoint,[Float])]
getSURF SURF{..} image = withNewMemory $ \ptr_mem ->
   with nullPtr $ \ptr_ptr_keypoints ->
   with nullPtr $ \ptr_ptr_descriptors ->
   with params  $ \ptr_params ->
   withImage image $ \ptr_image -> do
    ptr_keypoints' <- peek ptr_ptr_keypoints
    c'wrapExtractSURF (castPtr ptr_image) nullPtr ptr_ptr_keypoints
                      ptr_ptr_descriptors ptr_mem ptr_params 0
    ptr_keypoints <- peek ptr_ptr_keypoints
    ptr_descriptors <- peek ptr_ptr_descriptors
    a <- cvSeqToList ptr_keypoints
    b <- if extended
           then do
            es :: [FloatBlock128] <- cvSeqToList ptr_descriptors
            return (map (\(FP128 e) -> e) es)
           else do
            es :: [FloatBlock64] <- cvSeqToList ptr_descriptors
            return (map (\(FP64 e) -> e) es)
    return (zip a b)
   where
      params = C'CvSURFParams (if extended then 1 else 0)
                              hessianThreshold
                              nOctaves
                              nOctaveLayers

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

