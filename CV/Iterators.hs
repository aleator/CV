{-#LANGUAGE TypeFamilies, TypeSynonymInstances, ParallelListComp,
            FlexibleContexts, FlexibleInstances #-}
module CV.Iterators
( ImageContext(..)
, F32I
, filterPixels
, filterPixelsSlow
) where

import Data.Maybe
import Foreign.Ptr
import Foreign.ForeignPtr
import CV.Bindings.Types
import CV.Bindings.Iterators
import CV.Image
import C2HSTools

type F32I = Ptr C'F32_image_iterator

class ImageContext c where
  type V c :: *
  getPos :: c -> (Int,Int)
  getVal :: c -> Maybe (V c)
  evolve :: c -> c

instance ImageContext F32I where
  type V F32I = Float
  getPos c = unsafePerformIO $ do
    (C'CvPoint x y) <- peek p
    return $! (fromIntegral x, fromIntegral y)
    where
      p = c'F32_rowwise_pos c
  getVal c
    | p == nullPtr = Nothing
    | otherwise = unsafePerformIO $ do
        v <- peek p
        return $! Just (realToFrac v)
    where
      p = c'F32_val c
  evolve c = unsafePerformIO $ c'F32_next c

addPixelIf :: (Float -> Bool) -> [((Int,Int),Float)] -> F32I -> [((Int,Int),Float)]
addPixelIf cond ps c
  | isNothing v = ps
  | cond (fromJust v) = pair `seq` addPixelIf cond (pair : ps) $! evolve c
  | otherwise = addPixelIf cond ps $! evolve c
  where
        pos = getPos c
        pair = pos `seq` (pos, (fromJust v))
        v = getVal c

filterPixels :: (Float -> Bool) -> Image GrayScale D32 -> [((Int,Int),Float)]
filterPixels cond img =
  unsafePerformIO $
    withImage img $ \cimg -> do
      ptr <- c'alloc_F32_image_iterator
      if ptr == nullPtr
        then
        return []
        else do
          fptr <- newForeignPtr p'free_F32_image_iterator ptr
          withForeignPtr fptr $ \i -> do
            r <- c'F32_create_rowwise_iterator i cimg
            return $! addPixelIf cond [] i

-- slow method using list comprehensions, to verify iterator result...
filterPixelsSlow :: (Float -> Bool) -> Image GrayScale D32 -> [((Int,Int),Float)]
filterPixelsSlow cond img = filter (cond . snd) $ concat $ [ [((i,j),(v i j)) | j<-[0..height-1] ] | i <- [0..width-1] ]
  where
    v x y = getPixel (x,y) img
    (width,height) = getSize img
