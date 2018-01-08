{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  ( isMoveValid
  , isCoordStringValid
  , isCustodialCapturePossible
  , isFortifiedLambdaCapturePossible
  , getWinner
  ) where

import Tafl.Core

-- | Validates a string coordinate in algebraic notation.
isCoordStringValid :: String -> Bool
isCoordStringValid coord =
  (length coord) == 2
  && rowCode >= 97 && rowCode <= 105 && colCode >= 49 && colCode <= 57
    where
      rowCode = fromEnum $ head coord
      colCode = fromEnum $ (tail coord) !! 0 -- convert string to char

-- | Checks if a coordinate pair is on the board.
isCoordValid :: (Int, Int) -> Bool
isCoordValid (a, b) = a >= 0 && a <= 8 && b >= 0 && b <= 8

-- | Determines if a move is valid.
isMoveValid :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isMoveValid st (a, b) (x, y) =
  isPieceMovable st (a, b)
  && wouldPieceMove (a, b) (x, y)
  && isMoveStraight (a, b) (x, y)
  && isMoveUnobstructed st (a, b) (x, y)
  && not (isCastle (x, y))

-- | Determines if the current player is permitted to move the source piece.
isPieceMovable :: GameState -> (Int, Int) -> Bool
isPieceMovable st (a, b) =
  not (piece == Empty) &&
  if (currentPlayer st) == Objects
    then piece == Object
    else piece == Guard || piece == Lambda
  where
    piece = getSquare st (a, b)

-- | Determines if a move actually attempts to change a piece's location on the board.
wouldPieceMove :: (Int, Int) -> (Int, Int) -> Bool
wouldPieceMove (a, b) (x, y) = not (a == b) || not (x == y)

-- | Determines if a move from one set of coordinates to another is straight.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

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

-- | Determines if the given square is a castle.
isCastle :: (Int, Int) -> Bool
isCastle (x, y) = x == 4 && y == 4

-- | Checks if a piece can help the current player capture an enemy piece.
isPieceSupportive :: GameState -> (Int, Int) -> Bool
isPieceSupportive st (a, b) =
  isEmptyCastle ||
  if (currentPlayer st) == Objects
    then piece == Object
    else piece == Guard || piece == Lambda
  where
    isEmptyCastle = isCastle (a, b) && piece == Empty
    piece = getSquare st (a, b)

-- | Checks if a piece could be consumed by the current player.
isPieceConsumable :: GameState -> (Int, Int) -> Bool
isPieceConsumable st (a, b) =
  if (currentPlayer st) == Objects
    then piece == Guard || piece == Lambda
    else piece == Object
  where
    piece = getSquare st (a, b)

-- | Checks if a square contains a lambda in a fortified position (in or adjacent to castle).
isFortifiedLambda :: GameState -> (Int, Int) -> Bool
isFortifiedLambda st (a, b) = inFortifiedPosition && piece == Lambda
  where
    piece = getSquare st (a, b)
    inFortifiedPosition =
      (a, b) == (4, 4) ||
      (a, b) == (3, 4) ||
      (a, b) == (5, 4) ||
      (a, b) == (4, 3) ||
      (a, b) == (4, 5)

-- | Checks if a custodial capture is possible.
isCustodialCapturePossible :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isCustodialCapturePossible st (a, b) (x, y) =
  isCoordValid (a, b) &&
  isCoordValid (x, y) &&
  isPieceConsumable st (a, b) &&
  isPieceSupportive st (x, y) &&
  not (isFortifiedLambda st (a, b))

-- | Checks if the capture of a fortified lambda (in or adjacent to castle) is possible.
isFortifiedLambdaCapturePossible :: GameState -> (Int, Int) -> Bool
isFortifiedLambdaCapturePossible st (a, b) =
  (currentPlayer st) == Objects &&
  isFortifiedLambda st (a, b) &&
  isPieceSupportive st (a+1, b) &&
  isPieceSupportive st (a-1, b) &&
  isPieceSupportive st (a, b+1) &&
  isPieceSupportive st (a, b-1)

-- | Returns the winning player, if there is a winner.
getWinner :: GameState -> Maybe Player
getWinner st =
  if (haveAllObjectsBeenCaptured st) || (hasLambdaEscaped st)
    then Just Lambdas
  else if hasLambdaBeenCaptured st
    then Just Objects
  else
    Nothing

-- | Checks if all objects have been captured.
haveAllObjectsBeenCaptured :: GameState -> Bool
haveAllObjectsBeenCaptured st =
  foldl (\res sq -> res && not (sq == Object)) True joined
  where
    joined = foldl (\acc row -> acc ++ row) [] (board st)

-- | Checks if the lambda has reached an edge.
hasLambdaEscaped :: GameState -> Bool
hasLambdaEscaped st =
  (length escapedLambdas) > 0
    where
      -- index all squares with their coordinates
      indexed = [zip3 (fst x) (repeat (snd x)) [0..] | x <- zip (board st) [0..]]
      joined = foldl (\acc sq -> acc ++ sq) [] indexed
      -- apply filter to squares leaving only the escaped lambda
      isEscapedLambda :: (Square, Int, Int) -> Bool
      isEscapedLambda (sq, row, col) =
        sq == Lambda &&
        ((row == 0 || row == 8) || (col == 0 || col == 8))
      escapedLambdas = filter isEscapedLambda joined

-- | Checks if lambda has been captured.
hasLambdaBeenCaptured :: GameState -> Bool
hasLambdaBeenCaptured st =
  foldl (\res sq -> res && not (sq == Lambda)) True joined
  where
    joined = foldl (\acc row -> acc ++ row) [] (board st)

-- Determines if any moves are possible (draw).

-- Checks if a loaded game state is valid.
