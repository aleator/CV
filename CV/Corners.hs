module CV.Corners
( HarrisDesc
, Corner(..)
, ImageWithCorners(..)
, harris
, harrisCorners
) where

import CV.Bindings.Types
import CV.Bindings.Core
import CV.Bindings.ImgProc
import CV.Image
import CV.Operations
import CV.Iterators
import C2HSTools

type HarrisDesc = Float

data Corner d =
  Corner
  { pos :: (Int,Int)
  , desc :: d
  }

data ImageWithCorners d =
  ImageWithCorners
  { image :: Image GrayScale D32
  , corners :: [Corner d]
  }

harris :: Int -> Int -> Double -> Image GrayScale D32 -> Image GrayScale D32
harris bs as k src =
  unsafePerformIO $ do
    withCloneValue src $ \clone ->
        withGenImage src $ \si ->
          withGenImage clone $ \ci -> do
            c'cvCornerHarris si ci bs as k
            return clone

-- threshold for selecting harris corners
-- image _with_harris_applied_ (TODO: describe images with operations applied to them?)
-- result is an image normalize to [0..1] range, and a list of found corners
harrisCorners :: Float -> Image GrayScale D32 -> ImageWithCorners HarrisDesc
harrisCorners t src = (ImageWithCorners src cs)
  where
    cs = map (\(p,v) -> (Corner p v)) $ filterPixels (>t) $ normalize 1 0 NormMinMax $ harris 2 3 0.04 $ src
