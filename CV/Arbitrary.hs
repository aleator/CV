-- | This module provides QuickCheck generators for images.
module CV.Arbitrary (smallImage,constImage, noisyImage,smoothImage,blockNoise) where

import Control.Applicative 
import CV.Image
import CV.Pixelwise
import CV.Filters
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

newtype ZeroOne = ZO {unZo :: Float}
instance Arbitrary ZeroOne where
    arbitrary = ZO <$> choose (0,1)

-- | Generate a random small image, that might be constant, noisy or smoothly varying
--   Range of values is [0,1]
smallImage :: Gen (Image GrayScale D32)
smallImage = oneof [constImage, noisyImage,smoothImage]

-- | Generate 10x10 constant image
constImage :: Gen (Image GrayScale D32)
constImage = do
              ZO f <- arbitrary
              return $ toImage $ MkP {sizeOf = (10,10), eltOf = const f} 

-- | Generate 10x10 noisy image
noisyImage :: Gen (Image GrayScale D32)
noisyImage = do
              f <- arbitrary
              return $ toImage $ MkP {sizeOf = (10,10), eltOf = unZo . f} 

-- | Generate 10x10 smoothly varying image
smoothImage :: Gen (Image GrayScale D32)
smoothImage = gaussian (5,5) <$> noisyImage

blockNoise1  = montage (2,2) 0 <$> replicateM 4 smallImage

-- | Generate a (10m x 10m) sized noisy image.
blockNoise :: Int -> Gen (Image GrayScale D32)
blockNoise m = montage (m,m) 0 . replicate (m*m) <$> blockNoise1

