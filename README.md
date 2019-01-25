# Tetrihs - Tetris game in haskell
Tetrihs is the tetris game implemented in Haskell using CodeWorld library. The project arises as a task for the subject "Declarative Programming" from the Computer Science degree at the University of Seville.

## Features!

- Play tetris with 10 differents difficult levels
    1. Cheesy level
    2. Easy Peasy
    3. Easy
    4. Meh
    5. Okay
    6. Medium
    7. Hard
    8. Why are u running?
    9. Your actions have consequences
    10. AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
- Play in your favourite web browser (We don't expect that you use Internet Explorer)
- Keep a scoreboard and beat all the records.
---
## Structure

There are three principal modules.
- Blocks.hs: Contains all the functions and types related to the figures.
- Tetris.hs: Contains the entry point to the game.
- CodeWorldInterface.hs: Contains all the functions and structures needed to play with the CodeWorld interface
- Scores.hs: Contains all the functions related to the scores management.
---
## Installation

After clone the repository, follow these steps.

1. Execute ghci
2. Load the Tetris module with :l Tetris.hs
3. Execute tetris <level>
---
## How to play?

Use the following keys:

1. Down Arrow -> Move a figure down.
2. Right Arrow -> Move a figure to the right.
3. Left Arrow -> Move a figure to the left.
4. Ctrl -> Rotate a figure.
