{- |

This module defines the game's logic.

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

-- | Validate string coordinates in algebraic notation.
-- Return True if format is correct and coordinates lie on the board.
isCoordStringValid :: String -- coordinates string
                   -> Bool
isCoordStringValid coord =
  (length coord) == 2
  && rowCode >= 97 && rowCode <= 105 && colCode >= 49 && colCode <= 57
    where
      rowCode = fromEnum $ head coord
      colCode = fromEnum $ (tail coord) !! 0 -- convert string to char

-- | Return True if coordinates lie on the board.
isCoordValid :: (Int, Int) -- coordinates to be validated
             -> Bool
isCoordValid (a, b) = a >= 0 && a <= 8 && b >= 0 && b <= 8

-- | Return True if a move is valid.
isMoveValid :: GameState -- current game state
            -> (Int, Int) -- source coordinates
            -> (Int, Int) -- destination coordinates
            -> Bool
isMoveValid st (a, b) (x, y) =
  isPieceControllable st (a, b)
  && wouldPieceMove (a, b) (x, y)
  && isMoveStraight (a, b) (x, y)
  && isMoveUnobstructed st (a, b) (x, y)
  && not (isCastle (x, y))

-- | Return True if the current player is permitted to move the source piece.
isPieceControllable :: GameState -- current game state
                    -> (Int, Int) -- source coordinates
                    -> Bool
isPieceControllable st (a, b) =
  if (currentPlayer st) == Objects
    then piece == Object
    else piece == Guard || piece == Lambda
  where
    piece = getSquare st (a, b)

-- | Return True if the given coordinate points to an Empty Square.
isSquareEmpty :: GameState -- current game state
             -> (Int, Int) -- coordinates to be checked
             -> Bool
isSquareEmpty st (a, b) =
  piece == Empty
  where
    piece = getSquare st (a, b)

-- | Return True if a move actually attempts to
-- change a piece's location on the board.
wouldPieceMove :: (Int, Int) -- source coordinates
               -> (Int, Int) -- destination coordinates
               -> Bool
wouldPieceMove (a, b) (x, y) = not (a == b) || not (x == y)

-- | Return True if a move from one set of coordinates to another
-- is a straight line.
isMoveStraight :: (Int, Int) -- source coordinates
               -> (Int, Int) -- destination coordinates
               -> Bool
isMoveStraight (a, b) (x, y) = (a == x) || (b == y)

-- | Return True if there are any pieces obstructing a move.
isMoveUnobstructed :: GameState -- current game state
                   -> (Int, Int) -- source coordinates
                   -> (Int, Int) -- destination coordinates
                   -> Bool
isMoveUnobstructed st (a, b) (x, y) =
  -- check all squares in path are empty
  foldl (\res sq -> res && (sq == Empty)) True path
    where
      -- list of squares between src and dst, including dst
      path = if a == x
        -- +1 and -1 omits the src square from the path
        then [((board st) !! a) !! i | i <- [((min (b+1) y))..(max (b-1) y)]]
        else [((board st) !! i) !! b | i <- [((min (a+1) x))..(max (a-1) x)]]

-- | Return True if the given square is the castle.
isCastle :: (Int, Int) -- coordinates to be checked
         -> Bool
isCastle (x, y) = x == 4 && y == 4

-- | Return True if a piece can help the current player capture an enemy piece.
isPieceSupportive :: GameState -- current game state
                  -> (Int, Int) -- coordinates to be checked
                  -> Bool
isPieceSupportive st (a, b) =
  isEmptyCastle || (isPieceControllable st (a, b))
  where
    isEmptyCastle = (isCastle (a, b)) && (isSquareEmpty st (a, b))

-- | Return True if a piece could be consumed by the current player.
isPieceConsumable :: GameState -- current game state
                  -> (Int, Int) -- coordinates to be checked
                  -> Bool
isPieceConsumable st (a, b) =
  not (isPieceControllable st (a, b)) && not (isSquareEmpty st (a, b))

-- | Return True if a square contains a lambda
-- in a fortified position (in or adjacent to castle).
isFortifiedLambda :: GameState -- current game state
                  -> (Int, Int) -- coordinates to be checked
                  -> Bool
isFortifiedLambda st (a, b) = inFortifiedPosition && piece == Lambda
  where
    piece = getSquare st (a, b)
    inFortifiedPosition =
      (a, b) == (4, 4) ||
      (a, b) == (3, 4) ||
      (a, b) == (5, 4) ||
      (a, b) == (4, 3) ||
      (a, b) == (4, 5)

-- | Return True if a custodial capture is possible.
isCustodialCapturePossible :: GameState -- current game state
                           -> (Int, Int) -- coordinates of piece to be captured
                           -> (Int, Int) -- coordinates of supporting piece
                           -> Bool
isCustodialCapturePossible st (a, b) (x, y) =
  isCoordValid (a, b) &&
  isCoordValid (x, y) &&
  isPieceConsumable st (a, b) &&
  isPieceSupportive st (x, y) &&
  not (isFortifiedLambda st (a, b))

-- | Return True if the capture of a fortified lambda
-- (in or adjacent to castle) is possible.
isFortifiedLambdaCapturePossible :: GameState -- current game state
                                 -> (Int, Int) -- coordinates of piece to be captured
                                 -> Bool
isFortifiedLambdaCapturePossible st (a, b) =
  (currentPlayer st) == Objects &&
  isFortifiedLambda st (a, b) &&
  isPieceSupportive st (a+1, b) &&
  isPieceSupportive st (a-1, b) &&
  isPieceSupportive st (a, b+1) &&
  isPieceSupportive st (a, b-1)

-- | Returns the winning player, if there is a winner.
getWinner :: GameState -- current game state
          -> Maybe Player
getWinner st =
  if (haveAllObjectsBeenCaptured st) || (canLambdaEscape st)
    then Just Lambdas
  else if hasLambdaBeenCaptured st
    then Just Objects
  else
    Nothing

-- | Return True if the current player can move any of their pieces.
-- Used for checking draws.
canPlayerMove :: GameState -- current game state
              -> Bool
canPlayerMove st =
  foldl (||) False ownPiecesCanMove
  where
    positions = [(i, j) | i <- [0..8], j <- [0..8]]
    ownPiecePositions = filter (isPieceControllable st) positions
    ownPiecesCanMove = map (canPieceMove st) ownPiecePositions

-- | Return True if a piece is able to move in any direction.
canPieceMove :: GameState -- current game state
             -> (Int, Int) -- coordinates to be checked
             -> Bool
canPieceMove st (a, b) =
  (isCoordValid (a-1, b) && isMoveUnobstructed st (a, b) (a-1, b)) ||
  (isCoordValid (a+1, b) && isMoveUnobstructed st (a, b) (a+1, b)) ||
  (isCoordValid (a, b-1) && isMoveUnobstructed st (a, b) (a, b-1)) ||
  (isCoordValid (a, b+1) && isMoveUnobstructed st (a, b) (a, b+1))

-- | Return True if all objects have been captured.
haveAllObjectsBeenCaptured :: GameState -- current game state
                           -> Bool
haveAllObjectsBeenCaptured st =
  foldl (\res sq -> res && not (sq == Object)) True joined
  where
    -- join rows on board for simpler traversal
    joined = foldl (++) [] (board st)

-- | Return True if the lambda can reach the edge
-- of the board (only if it is the lambdas' turn).
canLambdaEscape :: GameState -- current game state
                -> Bool
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
    (a, b) = lambdaPositions !! 0 -- current position of lambda

-- | Return True if lambda has been captured.
hasLambdaBeenCaptured :: GameState -- current game state
                      -> Bool
hasLambdaBeenCaptured st =
  foldl (\res sq -> res && not (sq == Lambda)) True joined
  where
    -- join rows on board for simpler traversal
    joined = foldl (++) [] (board st)
