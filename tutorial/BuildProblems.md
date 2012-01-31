Problems Building?
==================

## CV/Image.chs.h:1: (column 1) [ERROR]  >>> Lexical error !
    The character '#' does not fit here.

This is probably due to misconfiguration of your system.
**Solution:** Build using LANG=C, for example:

~~~ {.bash}
> LANG=C cabal-dev install
~~~

## dist/build/CV/Video.chs.h:24: (column 32) [ERROR]  >>> Unknown identifier!
    Cannot find a definition for `CV_CAP_PROP_WHITE_BALANCE' in the header file.

This is due to using opencv library version greater than 2.2. To build with
2.3 or newer, supply the flag `-fopencv23` to cabal. For example:

**Solution:** 
~~~ {.bash}
cabal-dev install -fopencv23
~~~

## dist/build/CV/Bindings/Matrix.hs:1:1:
      File name does not match module name:
      Saw: `Main'
      Expected: `CV.Bindings.Matrix'

This problem is due installing bindings-dsl with cabal-dev. Or atleast
it can be fixed by a local install of bindings-dsl.

**Solution:** do `cabal install bindings-dsl` instead of 
`cabal-dev install bindings-dsl`


## "error while loading shared libraries: 
      libopencv_**.so.2.3: cannot open shared object file: 
      No such file or directory" when running software compiled
      with CV library.

This happens when opencv is installed outside your library path.
**Solution:**
 
  1. Find out the location of libopencv* shared objects. By default
     they are installed in `/usr/local/lib`
  2. Add this directory to `/etc/ld.so.conf`
  3. Run `ldconfig`
