# Tetris

Tetris built in Haskell using [Reactive-banana](https://wiki.haskell.org/Reactive-banana)
for FRP and [wxHaskell](https://wiki.haskell.org/WxHaskell) for GUI.

This is currently a work in progress but will include code for training a neural network to
play Pong as part of my final project for [Theory, Programming, and Simulation of Neuronal
Networks](http://www.vvz.ethz.ch/Vorlesungsverzeichnis/lerneinheitPre.do?semkez=2017S&ansicht=ALLE&lerneinheitId=112257&lang=en)
with [Reudi Stoop](https://www.ini.uzh.ch/people/ruedi) at [ETH Zürich](http://www.ethz.ch).

## Known issues

* Spurious segfaults. This seems to be related to drawing in wx. The frequency of segfaults has
been reduced by changing the code so as to draw less. Currently tolerable for demonstration purposes,
but not for "real" gameplay.

## Future improvements

* Play Tetris theme song in the background. Unclear why this doesn't seem to work.
* Show piece drop to bottom before rows clear
* Add delay before locking in pieces (to allow for horizontal movement after hitting the bottom)
* Increase levels
