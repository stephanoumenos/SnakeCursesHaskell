# SnakeCursesHaskell

This is a Snake games done with Haskell. I tried to make the code as clear as possible.

My main goal doing this was learning Haskell while doing it, and I feel it was great in that regard! Not only it was loads of fun, I came in contact with many of the language's features. I feel it was really helpful in doing so!

Any suggestions to improve the code are very welcome!

# How to run

First, make sure you have GHC (the haskell compiler) and Cabal (the haskell package manager). If you don't know how to install it refer to: https://www.haskell.org/downloads/linux

Then install the dependencies:

```
cabal install c2hs
cabal install NCurses
```

Compile the program:

```
ghc --make -O2 Snake
```

Run the game:

```
./Snake
```

# Controls


Up: w, Up Arrow or k

Down: s, Down Arrow or j

Left: a, Left Arrow or h

Right: d, Right Arrow or l
