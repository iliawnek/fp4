{- |

The `Process` module implements the game commands.

-}
module Tafl.Process
  ( processCommand
  , processCommandStr
  , printError
  , printMoveInfo
  , makeMove
  ) where

import System.Exit
import Data.Typeable

import Tafl.Core
import Tafl.Logic
import Tafl.File

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
  -- TODO: reset game state
  let newSt = st {inGame=False}
  putStrLn "Stopping Game."
  pure $ Right newSt

processCommand st (Move src dst) = do
  let newSt = makeMove st src dst
  case newSt of
    (Left err) -> pure $ Left err
    (Right st) -> do
      putStrLn "Move Successful"
      let winner = getWinner st
      -- TODO: reset game state instead of ending the program.
      case winner of
        Just Lambdas -> do
          putStrLn "Lambdas Win"
          exitWith ExitSuccess
        Just Objects -> do
          putStrLn "Objects Win"
          exitWith ExitSuccess
        Nothing -> pure $ newSt

processCommand st (Save fname) = do
  saveGameState st fname
  pure $ Right st

processCommand st (Load fname) = do
  loadGameState fname
  pure $ Right st

-- The remaining commands are to be added here.

processCommand st _ = pure $ Left (UnknownCommand)

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
  putStrLn $ unlines [unwords [squareToSymbol ((board st !! y) !! x) | x <- [0..8]] | y <- [0..8]]

-- | Convert coordinates in algebraic notation into indices appropriate for the board data structure.
parseCoord :: String -> (Int, Int)
parseCoord (col:row) = (rowIndex, colIndex)
  where
    rowIndex = 8 - ((read row :: Int) - 1)
    colIndex = (fromEnum col) - 97

-- | Move a piece on the board from one square to another.
-- TODO: return error
makeMove :: GameState -> String -> String -> Either TaflError GameState
makeMove st src dst =
  if (not $ inGame st)
    then Left CurrentlyUnusableCommand
  else if not (isCoordStringValid src) || not (isCoordStringValid dst)
    then Left InvalidMove
  else if (not $ isMoveValid st (a, b) (x, y))
    then Left InvalidMove
  else
    Right newSt
  where
    (a, b) = parseCoord src
    (x, y) = parseCoord dst
    srcSquare = getSquare st (a, b)
    dstSquare = getSquare st (x, y)
    newStAfterMove = setSquare (setSquare st (a, b) Empty) (x, y) srcSquare
    newStAfterCapture = removeCaptures newStAfterMove (x, y)
    newSt = switchPlayer newStAfterCapture

-- | Remove all captured pieces.
removeCaptures :: GameState -> (Int, Int) -> GameState
removeCaptures st (x, y) =
  custodialCapture (x+1, y) (x+2, y) $
  custodialCapture (x-1, y) (x-2, y) $
  custodialCapture (x, y+1) (x, y+2) $
  custodialCapture (x, y-1) (x, y-2) $
  fortifiedLambdaCapture (x+1, y) $
  fortifiedLambdaCapture (x-1, y) $
  fortifiedLambdaCapture (x, y+1) $
  fortifiedLambdaCapture (x, y-1) $
  st

-- | Perform a custodial capture if the correct conditions are met.
custodialCapture :: (Int, Int) -> (Int, Int) -> GameState -> GameState
custodialCapture (a, b) (x, y) st =
  if isCustodialCapturePossible st (a, b) (x, y)
  then newSt
  else st
  where
    newSt = setSquare st (a, b) Empty

-- | Perform a fortified lambda capture if the correct conditions are met.
fortifiedLambdaCapture :: (Int, Int) -> GameState -> GameState
fortifiedLambdaCapture (a, b) st =
  if isFortifiedLambdaCapturePossible st (a, b)
  then newSt
  else st
  where
    newSt = setSquare st (a, b) Empty

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
printError (InvalidMove) = do
  putStrLn "Invalid Move!"
printError (CurrentlyUnusableCommand) = do
  putStrLn "The command cannot be used."
