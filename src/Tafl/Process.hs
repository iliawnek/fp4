{- |

The `Process` module implements the game commands.

-}
module Tafl.Process
  ( processCommand
  , processCommandStr
  , printError
  , printMoveInfo
  , makeMove -- TODO: remove?
  ) where

import System.Exit

import Tafl.Core
import Tafl.Logic

-- | Process user commands and updates the GameState.
-- Returns a `TaflError`
processCommand :: GameState
               -> Command
               -> IO (Either TaflError GameState)
processCommand st Help = do
  putStrLn help_text
  pure $ Right st
processCommand st Exit = do
  putStrLn "Good Bye!"
  exitWith ExitSuccess

processCommand st Start = do
  let newSt = st {inGame=True}
  putStrLn "Starting Game."
  pure $ Right newSt

processCommand st Stop = do
  let newSt = st {inGame=False}
  putStrLn "Stopping Game."
  pure $ Right newSt

processCommand st (Move src dst) = do
  let newSt = makeMove st src dst
  case newSt of
    (Left err) -> pure $ Left err
    (Right st) -> pure $ newSt

-- The remaining commands are to be added here.

processCommand st _ = pure $ Left (UnknownCommand)

-- | A character to represent a piece on the board.
printSquare :: Square -> String
printSquare Empty = " "
printSquare Object = "O"
printSquare Lambda = "L"
printSquare Guard = "G"

-- | Print all relevant info regarding the next move.
printMoveInfo :: GameState -> IO ()
printMoveInfo st = do
  printBoard st
  printCurrentPlayer st

-- | Print a message indicating which player must make the next move.
printCurrentPlayer :: GameState -> IO ()
printCurrentPlayer st = do
  putStr $ show $ currentPlayer st
  putStrLn " make the next move..."

-- | Print the board in its current state.
printBoard :: GameState -> IO ()
printBoard st = do
  putStr "\n"
  putStrLn $ unlines [unwords [printSquare ((board st !! y) !! x) | x <- [0..8]] | y <- [0..8]]

-- | Convert coordinates in algebraic notation into indices appropriate for the board data structure.
-- TODO: return error if input is not coordinate, or if coordinate is off the board
-- TODO: handle capital letters?
parseCoordinateString :: String -> (Int, Int)
parseCoordinateString (column:row) = (rowIndex, colIndex)
  where
    rowIndex = 8 - ((read row :: Int) - 1)
    colIndex = (fromEnum column) - 97

-- | Get the contents of a specific square on the board.
getSquare :: GameState -> (Int, Int) -> Square
getSquare st (rowIndex, colIndex) = ((board st) !! rowIndex) !! colIndex

-- | Set the contents of a specific square on the board.
setSquare :: GameState -> (Int, Int) -> Square -> GameState
setSquare st (rowIndex, colIndex) replacement = st {board = newBoard}
  where
    replaceSquare = \(square, index) -> if (index == colIndex) then replacement else square
    replaceRow = \(row, index) -> if (index == rowIndex) then (map replaceSquare (zip row [0..])) else row
    newBoard = map replaceRow (zip (board st) [0..])

-- | Move a piece on the board from one square to another.
-- TODO: return error
makeMove :: GameState -> String -> String -> Either TaflError GameState
makeMove st src dst =
  if (not $ inGame st)
    then Left CurrentlyUnusableCommand
  else if (not $ isMoveStraight (iSrcRow, iSrcCol) (iDstRow, iDstCol))
    then Left IllegalMove
  else
    Right newSt
  where
    (iSrcRow, iSrcCol) = parseCoordinateString src
    (iDstRow, iDstCol) = parseCoordinateString dst
    srcSquare = getSquare st (iSrcRow, iSrcCol)
    dstSquare = getSquare st (iDstRow, iDstCol)
    newSt = if (dstSquare == Empty)
      then setSquare (setSquare st (iSrcRow, iSrcCol) Empty) (iDstRow, iDstCol) srcSquare
      else st

-- | Process a user given command presented as a String, and update
-- the GameState.
processCommandStr :: GameState
                  -> String
                  -> IO (Either TaflError GameState)
processCommandStr st str =
  case commandFromString str of
    Left err   -> pure (Left err)
    Right cmd' -> processCommand st cmd'


-- | Print an Error to STDOUT.
printError :: TaflError -> IO ()
printError (NotYetImplemented) = do
  putStrLn "Not Yet Implemented."
printError (MalformedCommand) = do
  putStrLn "The entered command was malformed."
printError (UnknownCommand) = do
  putStrLn "The entered command was not recognised."
printError (IllegalMove) = do
  putStrLn "The entered move is illegal."
printError (CurrentlyUnusableCommand) = do
  putStrLn "The command cannot be used."
