{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  ( isMoveValid
  , isCoordStringValid
  , isCustodialCapturePossible
  , isFortifiedLambdaCapturePossible
  , getWinner
  , canPlayerMove
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

-- | Checks if a move is valid.
isMoveValid :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isMoveValid st (a, b) (x, y) =
  isPieceControllable st (a, b)
  && wouldPieceMove (a, b) (x, y)
  && isMoveStraight (a, b) (x, y)
  && isMoveUnobstructed st (a, b) (x, y)
  && not (isCastle (x, y))

-- | Checks if the current player is permitted to move the source piece.
isPieceControllable :: GameState -> (Int, Int) -> Bool
isPieceControllable st (a, b) =
  if (currentPlayer st) == Objects
    then piece == Object
    else piece == Guard || piece == Lambda
  where
    piece = getSquare st (a, b)

isPieceEmpty :: GameState -> (Int, Int) -> Bool
isPieceEmpty st (a, b) =
  piece == Empty
  where
    piece = getSquare st (a, b)

-- | Checks if a move actually attempts to change a piece's location on the board.
wouldPieceMove :: (Int, Int) -> (Int, Int) -> Bool
wouldPieceMove (a, b) (x, y) = not (a == b) || not (x == y)

-- | Checks if a move from one set of coordinates to another is straight.
isMoveStraight :: (Int, Int) -> (Int, Int) -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

-- | Checks if there are any pieces obstructing a move.
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

-- | Checks if the given square is the castle.
isCastle :: (Int, Int) -> Bool
isCastle (x, y) = x == 4 && y == 4

-- | Checks if a piece can help the current player capture an enemy piece.
isPieceSupportive :: GameState -> (Int, Int) -> Bool
isPieceSupportive st (a, b) =
  isEmptyCastle || (isPieceControllable st (a, b))
  where
    isEmptyCastle = (isCastle (a, b)) && (isPieceEmpty st (a, b))

-- | Checks if a piece could be consumed by the current player.
isPieceConsumable :: GameState -> (Int, Int) -> Bool
isPieceConsumable st (a, b) =
  not (isPieceControllable st (a, b)) && not (isPieceEmpty st (a, b))

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
  if (haveAllObjectsBeenCaptured st) || (canLambdaEscape st)
    then Just Lambdas
  else if hasLambdaBeenCaptured st
    then Just Objects
  else
    Nothing

-- | Checks if the current player can move any of their pieces.
canPlayerMove :: GameState -> Bool
canPlayerMove st =
  foldl (\acc canMove -> acc || canMove) False ownPiecesCanMove
  where
    positions = [(i, j) | i <- [0..8], j <- [0..8]]
    ownPiecePositions = filter (\p -> isPieceControllable st p) positions
    ownPiecesCanMove = map (\p -> canPieceMove st p) ownPiecePositions

canPieceMove :: GameState -> (Int, Int) -> Bool
canPieceMove st (a, b) =
  (isCoordValid (a-1, b) && isMoveUnobstructed st (a, b) (a-1, b)) ||
  (isCoordValid (a+1, b) && isMoveUnobstructed st (a, b) (a+1, b)) ||
  (isCoordValid (a, b-1) && isMoveUnobstructed st (a, b) (a, b-1)) ||
  (isCoordValid (a, b+1) && isMoveUnobstructed st (a, b) (a, b+1))

-- | Checks if all objects have been captured.
haveAllObjectsBeenCaptured :: GameState -> Bool
haveAllObjectsBeenCaptured st =
  foldl (\res sq -> res && not (sq == Object)) True joined
  where
    joined = foldl (\acc row -> acc ++ row) [] (board st)

-- | Checks if the lambda can reach the edge of the board (only if it is the lambdas' turn).
canLambdaEscape :: GameState -> Bool
canLambdaEscape st =
  (currentPlayer st) == Lambdas &&
  (length lambdaPositions) == 1 &&
  ( isMoveUnobstructed st (a, b) (0, b) ||
    isMoveUnobstructed st (a, b) (8, b) ||
    isMoveUnobstructed st (a, b) (a, 0) ||
    isMoveUnobstructed st (a, b) (a, 8)  )
  where
    positions = [(i, j) | i <- [0..8], j <- [0..8]]
    lambdaPositions = filter (\p -> (getSquare st p) == Lambda) positions
    (a, b) = lambdaPositions !! 0

-- | Checks if lambda has been captured.
hasLambdaBeenCaptured :: GameState -> Bool
hasLambdaBeenCaptured st =
  foldl (\res sq -> res && not (sq == Lambda)) True joined
  where
    joined = foldl (\acc row -> acc ++ row) [] (board st)
