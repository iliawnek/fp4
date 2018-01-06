{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  ( isMoveStraight
  ) where

import Tafl.Core

-- Determines if a move is legal.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (iSrcRow, iSrcCol) (iDstRow, iDstCol) = (iSrcRow == iDstRow) || (iSrcCol == iDstCol)

-- Determines if a loaded game state is valid.

-- Determines which opposing pieces a move consumes.

-- Determines if any moves are possible.

-- Determines if the lambda has reached an edge.

-- Determines if the current player is permitted to move the selected piece.

-- Determines if the opposing player has no more pieces.
