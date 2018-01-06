{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  ( isMoveValid
  ) where

import Tafl.Core

-- | Determines if a move is valid.
isMoveValid :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isMoveValid st (a, b) (x, y) =
  isMoveStraight (a, b) (x, y)
  && isDestinationEmpty st (x, y)

-- Determines if a move from one set of coordinates to another is straight.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

-- Determines if a move's destination is not currently occupied by another piece.
isDestinationEmpty :: GameState -> (Int, Int) -> Bool
isDestinationEmpty st (x, y) = getSquare st (x, y) == Empty

-- Determines if a loaded game state is valid.

-- Determines which opposing pieces a move consumes.

-- Determines if any moves are possible.

-- Determines if the lambda has reached an edge.

-- Determines if the current player is permitted to move the selected piece.

-- Determines if the opposing player has no more pieces.
