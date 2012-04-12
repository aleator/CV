-- | This module is an applicative wrapper for images. It introduces Pixelwise type that
--   can be converted from and to grayscale images and which has an applicative and functor
--   instances.
{-#LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes#-}
module CV.Pixelwise (Pixelwise(..)
                    ,fromImage
                    ,fromFunction
                    ,toImage
                    ,toImagePar
                    ,remap
                    ,remapImage
                    ,mapImage
                    ,mapPixels
                    ,imageFromFunction
                    ,imageToFunction
                    ,(<$$>)
                    ,(<+>)) where
import Control.Applicative
import CV.Image
import System.IO.Unsafe
import Control.Concurrent.ParallelIO.Local

-- | A wrapper for allowing functor and applicative instances for non-polymorphic image types.
data Pixelwise x = MkP {sizeOf :: (Int,Int)
                       ,eltOf  :: (Int,Int) -> x}

instance GetPixel (Pixelwise x) where
    type P (Pixelwise x) = x
    getPixel p (MkP s e) = e p

instance Sized (Pixelwise a) where
    type Size (Pixelwise a) = (Int,Int)
    getSize (MkP s _) = s

instance Functor Pixelwise where
    fmap f (MkP s e) = MkP s (fmap f e)

instance Applicative Pixelwise where
  pure x               = MkP maxBound  (pure x)
  MkP i f <*> MkP j g  = MkP (min i j) (f <*> g)

instance (Eq a) => Eq (Pixelwise a) where
    (MkP (s1@(w1,h1)) e1)  == (MkP s2 e2)
        = s1==s2 && and [e1 (i,j) == e2 (i,j) | i <- [0..w1-1] , j <- [0..h1-1]]

instance Show (Pixelwise a) where
    show (MkP s1 e1)
        = "MkP "++show (s1)++" <image-data>"

instance (Num a) => Num (Pixelwise a) where
  a + b = (+) <$> a <*> b
  a * b = (*) <$> a <*> b
  a - b = (-) <$> a <*> b
  negate = fmap negate
  abs    = fmap abs
  signum = error "Signum is undefined for images"
  fromInteger i = MkP{sizeOf = (1,1),eltOf=const (fromIntegral i)}

withPixels x = toImage . x . fromImage

-- | Re-arrange pixel positions and values
remap :: (((Int,Int) -> b) -> ((Int,Int) -> x)) -> Pixelwise b -> Pixelwise x
remap f (MkP s e) = MkP s (f e)

-- | Convert image to a function, which returns pixel values in the domain of
-- the image and zero elsewhere
imageToFunction :: (GetPixel (Image a b), Num (P (Image a b)))
                  => Image a b -> ((Int,Int) -> P (Image a b))
imageToFunction img (x,y) | x>= 0 && y>= 0 && x < w && y < h = getPixel (x,y) img
                          | otherwise = 0
   where (w,h) = getSize img

fromImage :: (Num (P b), GetPixel b, Sized b, Size b ~ Size (Pixelwise (P b))) => b -> Pixelwise (P b)
fromImage i = MkP size getP
    where
        size@(w,h) = getSize i
        getP (x,y) | x>=0 && x<w && y>=0 && y<h = getPixel (x,y) i
                   | otherwise = 0

-- | Convert a pixelwise construct to image.
toImagePar :: (SetPixel (Image a b), CreateImage (Image a b))
           => Int -> Pixelwise (SP (Image a b)) -> Image a b
toImagePar n (MkP (w,h) e) = unsafePerformIO $ withPool n $ \pool -> do
        img <- create (w,h)
        parallel_ pool [sequence_ [setPixel (i,j) (e (i,j)) img | i <- [0..w-1]]
                       |j <- [0..h-1]]
        return img

-- | Convert a pixelwise construct to image.
toImage :: (SetPixel (Image a b), CreateImage (Image a b))
           => Pixelwise (SP (Image a b)) -> Image a b
toImage (MkP (w,h) e) = unsafePerformIO $ do
        img <- create (w,h)
        sequence_ [setPixel (i,j) (e (i,j)) img
                  | i <- [0..w-1]
                  , j <- [0..h-1]
                  ]
        return img

remapImage ::
     (CreateImage (Image a b),
      SetPixel (Image a b),
      Num (P (Image  a b)),
      GetPixel (Image a b)) =>
     (((Int, Int) -> P (Image a b)) -> (Int, Int) -> SP (Image a b)) -> Image a b -> Image a b

remapImage f i = toImage . remap f $ fromImage i
-- toImage . remap f . fromImage . toImage . remap f . fromImage

mapPixels :: (t -> x) -> Pixelwise t -> Pixelwise x
mapPixels f (MkP s e) = MkP s (\(i,j) -> f $ e (i,j))

mapImage ::
     (CreateImage (Image c d),
      SetPixel (Image c d),
      Num (P (Image a b)),
      GetPixel (Image a b)) =>
     (P (Image a b) -> SP (Image c d)) -> Image a b -> Image c d
mapImage f = toImage . mapPixels f . fromImage


-- | Convert a function into construct into a Pixelwise construct
fromFunction :: (Int, Int) -> ((Int, Int) -> x) -> Pixelwise x
fromFunction size f = MkP size f

imageFromFunction :: (SetPixel (Image a b), CreateImage (Image a b)) =>
                     (Int,Int) -> ((Int,Int) -> (SP (Image a b))) -> Image a b
imageFromFunction size = toImage . fromFunction size


-- | Shorthand for `a <$> fromImage b`
(<$$>) :: (Num (P b1), Size b1 ~ (Int, Int), Sized b1, GetPixel b1) => (P b1 -> b) -> b1 -> Pixelwise b
a <$$> b = a <$> fromImage b

-- | Shorthand for `a <*> fromImage b`
(<+>) :: (Num (P b1),Size b1 ~ (Int, Int), Sized b1, GetPixel b1) => Pixelwise (P b1 -> b) -> b1 -> Pixelwise b
a <+> b = a <*> fromImage b
