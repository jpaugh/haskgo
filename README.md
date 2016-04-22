# HaskGo

> A Haskell-based Go Engine

The game of *weiqi*, otherwise known as *Go*, is this. This project is
alpha-quality, so expect everything to be broken, missing, or confusing.
;-) If you have any questions, please open an issue, or leave a comment.

If you'd like to contribute, please read CONTRIBUTORS, add your name
there, and submit a patch! This project is licensed under the MIT
license (See LICENSE for details).

# Building

The easiest way to build is to use docker:

    $ docker build -t jpaugh/haskgo . && docker run --rm -it -v "$PWD":/opt/server jpaugh/haskgo

This will eventually open up a ghci session, ready for you to start
hacking! (The volume mount lets GHCI pick up your changes to the source
on the local file-system; the next time you do a docker build, those
changes will be included in the new image)

Of course, if you don't have docker available, then you can build and
run it in the usual way:

    cabal sandbox init
    cabal install --only-dependencies -j
    cabal repl
