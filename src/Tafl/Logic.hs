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
  && isCurrentPlayersPiece st (a, b)
  && isMoveUnobstructed st (a, b) (x, y)

-- is src not empty?

-- are src and dst different?

-- | Determines if a move from one set of coordinates to another is straight.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

-- | Determines if the piece to be moved belongs to the current player.
isCurrentPlayersPiece :: GameState -> (Int, Int) -> Bool
isCurrentPlayersPiece st (a, b) =
  if ((currentPlayer st) == Objects)
    then getSquare st (a, b) == Object
    else getSquare st (a, b) == Guard || getSquare st (a, b) == Lambda

-- | Determines if there are any pieces obstructing a move.
isMoveUnobstructed :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isMoveUnobstructed st (a, b) (x, y) =
  -- check all squares in path are empty
  foldl (\res sq -> res && (sq == Empty)) True path
    where
      -- list of squares between src and dst, including dst
      path = if a == x
        -- +1 and -1 omits the src square from the path
        then [((board st) !! a) !! i | i <- [((min (b+1) y))..(max (b-1) y)]]
        else [((board st) !! i) !! b | i <- [((min (a+1) x))..(max (a-1) x)]]


-- | Determines if a move does not enter the castle.
-- isMoveOutsideCastle

-- Determines if a loaded game state is valid.

-- Determines which opposing pieces a move consumes.

-- Determines if any moves are possible.

-- Determines if the lambda has reached an edge.

-- Determines if the current player is permitted to move the selected piece.

-- Determines if the opposing player has no more pieces.
