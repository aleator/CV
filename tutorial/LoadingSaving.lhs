> module Main where

Loading and Saving Images
=========================

The basic functionality of CV module is contained in the CV.Image module which
provides wrappers for basic opencv image type. Unlike in C, the image type is parametrized
over the color and bit-depth of the image, which allows us to avoid several runtime errors.
For example, we assert that variable `foo` contains an 8-bit grayscale image like this:

~~~{.haskell}
foo :: Image GrayScale D8
~~~


This module also contains the auxiliary functions for basic image IO. As the hello world
example, we can load an image, apply an edge enhancing filter on it and save the result to
a png file. To do this, we begin by importing the relevant modules and declaring a main
function:

> import CV.Image
> import CV.Edges

> main = do
>   image <- readFromFile "smallLena.jpg" :: IO (Image GrayScale D32)
>   let result = sobel (1,0) s5 image
>   saveImage "Result.png" result

To read an image file, we must often give it an explicit type 
declaration (`Image GrayScale D32`). In this case we load the image as
a grayscale image with pixels represented as 32-bit floating point numbers,
which happens to be the image type that our edge enhancing `sobel` function
accepts.

The resulting image is saved with the function `saveImage`, which detects
the fileformat from the given filename. Unfortunately however,
opencv itself will abort if the the filename doesn't contain a suitable extension,
or if opencv was compiled without support for the specific image type, resulting
in a runtime error. 

This program can now be compiled and executed

~~~{.bash}
@ ghc --make LoadingSaving.hs
@ ./LoadingSaving
~~~

and it will produce file `Result.png` which contains this image:
![Edge detector applied to lena-image](Result.png)



