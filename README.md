# Chinese checkers

A Chinese checkers engine written in functional OCaml. It handles the board, moves, multiple jumps, winner detection and full game simulation.

## The board

The six-pointed star board uses cube coordinates `(i, j, k)` with `i + j + k = 0`. This choice makes a lot of things simple: a 60° rotation is a permutation of the coordinates with sign changes, two cells are aligned when they share a coordinate, and a move is a vector addition.

## What's implemented

- Geometry: membership in the diamond and the star, rotation, translation, neighbors, pivot cell of a jump, vector and distance between two cells
- Starting position for 1, 2, 3 or 6 players, built by rotating the first player's triangle
- ASCII display of the board in the terminal
- Moves:
  - `Du (c1, c2)`: move by one cell
  - jump over a piece, symmetric with respect to the jumped piece (the path must be clear)
  - `Sm [c1; c2; …]`: several chained jumps
- Move validation, then board rotation to hand over to the next player
- Score of each player and win detection
- Simulation of a full game from a list of moves, returning the winner

Every function has its own `assert` tests. Identifiers and comments in the code are in French.

## Running

```bash
ocaml dames_chinoises.ml
```

or from the OCaml toplevel:

```ocaml
#use "dames_chinoises.ml";;
```

When the file loads, the tests run and a few example boards are printed.

## Concepts used

Sum and product types, pattern matching, recursion on lists, higher-order functions (`List.map`, `List.fold_right`), the `option` type.

## Author

Karim Glelah
