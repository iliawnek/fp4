{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  ( isMoveValid
  , isCoordValid
  ) where

import Tafl.Core

-- | Validates a string coordinate in algebraic notation.
isCoordValid :: String -> Bool
isCoordValid coord =
  (length coord) == 2
  && rowCode >= 97 && rowCode <= 105 && colCode >= 49 && colCode <= 57
    where
      rowCode = fromEnum $ head coord
      colCode = fromEnum $ (tail coord) !! 0 -- convert string to char

-- | Determines if a move is valid.
isMoveValid :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isMoveValid st (a, b) (x, y) =
  isMoveStraight (a, b) (x, y)
  && isDestinationEmpty st (x, y)
  && isCurrentPlayersPiece st (a, b)

-- | Determines if a move from one set of coordinates to another is straight.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

-- | Determines if a move's destination is not currently occupied by another piece.
isDestinationEmpty :: GameState -> (Int, Int) -> Bool
isDestinationEmpty st (x, y) = getSquare st (x, y) == Empty

-- | Determines if the moved piece belongs to the current player.
isCurrentPlayersPiece :: GameState -> (Int, Int) -> Bool
isCurrentPlayersPiece st (a, b) =
  if ((currentPlayer st) == Objects)
    then getSquare st (a, b) == Object
    else getSquare st (a, b) == Guard || getSquare st (a, b) == Lambda

-- | Determines if a move does not jump over another piece.
-- isMoveUnblocked :: GameState -> (Int, Int) -> (Int, Int) -> Bool
-- TODO

-- | Determines if a move does not enter the castle.
-- isMoveOutsideCastle

-- Determines if a loaded game state is valid.

-- Determines which opposing pieces a move consumes.

-- Determines if any moves are possible.

-- Determines if the lambda has reached an edge.

-- Determines if the current player is permitted to move the selected piece.

-- Determines if the opposing player has no more pieces.
