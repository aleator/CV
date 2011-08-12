-- | This module is an applicative wrapper for images. It introduces Pixelwise type that
--   can be converted from and to grayscale images and which has an applicative and functor
--   instances.
{-#LANGUAGE TypeFamilies#-}
module CV.Pixelwise (Pixelwise, fromImage, toImage, (<$$>),(<+>)) where
import Control.Applicative 
import CV.Image
import System.IO.Unsafe

-- | A wrapper for allowing functor and applicative instances for non-polymorphic image types.
data Pixelwise x = MkP {sizeOf :: (Int,Int)
                       ,eltOf :: (Int,Int) -> x}

instance GetPixel (Pixelwise x) where
    type P (Pixelwise x) = x
    getPixel p (MkP s e) = e p

instance IntSized (Pixelwise a) where
    getSize (MkP s _) = s 


instance Functor Pixelwise where
    fmap f (MkP s e) = MkP s (fmap f e)

instance Applicative Pixelwise where
  pure x               = MkP maxBound  (pure x)
  MkP i f <*> MkP j g  = MkP (min i j) (f <*> g)

-- | Convert a pixelwise construct into an image.
fromImage :: (GetPixel b, IntSized b) => b -> Pixelwise (P b)
fromImage i = MkP (getSize i) (flip getPixel $ i)

-- | Convert an image to pixelwise construct.
toImage :: Pixelwise D32 -> Image GrayScale D32
toImage (MkP (w,h) e) = unsafePerformIO $ do
        img <- create (w,h)
        sequence_ [setPixel (i,j) (e (i,j)) img
                  | i <- [0..w-1]
                  , j <- [0..h-1]
                  ]
        return img

-- | Shorthand for a <$> fromImage b
a <$$> b = a <$> fromImage b
-- | Shorthand for a <*> fromImage b
a <+> b = a <*> fromImage b
