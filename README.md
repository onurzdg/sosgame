SOS Game
------------
This a simple text-based [SOS](https://en.wikipedia.org/wiki/SOS_(game)) game written in Haskell.

Gameplay
----

When it's your turn, first simply input the 
piece (S or O) you want to place on the board and then press
"enter". 
Next, tell where you want to place the piece like so: 5 A (row 5, column A)



Notes
----
Letter E stands for "empty spot".



Running the project
----------

1. ``cabal sandbox init``
2. ``cabal install --dependencies-only && cabal configure && cabal build``
3. ``cabal run``


Enjoy the game!
