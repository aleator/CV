About 
=====

This is a machine vision package that is implemented on top of the [OpenCV C library]
library. 

How To Install
==============

The main installation instructions are detailed in the [Project Wiki]. In short, install
GHC >= 7.4.1, cabal install, c2hs and c version of opencv and your good to go.
According to the OpenCV version you are using, you may need to run the installation
process with one of these two flags ```-fopencv23``` or ```-fopencv24```, like so:

```
cabal install -fopencv24
```

There is also an [example installation transcript with all the gory details] in the wiki.

[example installation with all the gory details]: https://github.com/aleator/CV/wiki/One-go-at-installing-CV

How To Use
==========

The Documentation is available at <http://aleator.github.com/CV/> 

Screenshots? Examples?
======================

There are some screenshots at <http://aleator.github.com/CV/> and the [`examples`] directory of
this project contains a plenty of small sample programs.

[`examples`]:https://github.com/aleator/CV/tree/master/examples
[OpenCV C library]: http://opencv.willowgarage.com

Changelog
---------

 *  0.3.0.2 - Workaround for compiling with OS X 10.6 & fixed errors
              about M_PI .
