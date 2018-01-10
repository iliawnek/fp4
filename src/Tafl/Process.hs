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
import Data.List

import Tafl.Core
import Tafl.Logic

-- | Process user commands and updates the GameState.
-- Returns a `TaflError` if any command results in a error.
processCommand :: GameState -- current game state
               -> Command -- command to be processed
               -> IO (Either TaflError GameState) -- game state after applying the command
-- | Display help text containing all supported commands.
processCommand st Help = do
  putStrLn helpText
  pure $ Right st
-- | Exit the program.
processCommand st Exit = do
  putStrLn "Good Bye!"
  exitWith ExitSuccess
-- | Start a new game.
processCommand st Start = do
  let newSt = st {inGame=True}
  putStrLn "Starting Game."
  pure $ Right newSt
-- | Stop and clear an ongoing game.
processCommand st Stop = do
  putStrLn "Stopping Game."
  initGameState Nothing (inTestMode st) -- reset game state
-- | Make a move from src coordinate to dst coordinate.
-- Stop the game if the move results in a win or draw.
-- Return a TaflError if the move is invalid.
processCommand st (Move src dst) = do
  let result = makeMove st src dst
  case result of
    (Left err) -> pure $ Left err
    (Right newSt) -> do
      putStrLn "Move Successful"
      checkEndGame newSt
-- | Save the current game state to an file called `fname`.
-- Return a TaflError if the state cannot be saved.
processCommand st (Save fname) = do
  result <- saveGameState st fname
  case result of
    Just err -> pure $ Left err
    Nothing -> do
      putStrLn $ "State saved in " ++ fname
      pure $ Right st
-- | Load a game state from a file called `fname`.
-- Return a TaflError if the game state is malformed.
-- Return a TaflError if the file cannot be accessed.
processCommand st (Load fname) = do
  result <- loadGameState st fname
  case result of
    (Left err) -> pure $ Left err
    (Right newSt) -> checkEndGame newSt

-- | Return a TaflError for all unrecognised commands.
processCommand st _ = pure $ Left (UnknownCommand)

-- | Generate help text displaying the game's supporting commands.
helpText :: String
helpText = unlines $
     [ "Tafl Help text:", ""]
  ++ map prettyCmdHelp
       [ ("help", "Displays this help text.")
       , ("exit", "Exits the Command Prompt.")
       , ("start", "Initiates a game.")
       , ("stop", "Stops a game.")
       , ("move <src> <dst>", "Moves a piece from src to dst, where src and dst are defined using algebraic notation (e.g. a1, e5, i9).")
       , ("save <fname>", "Save the current game state to the file at <fname>.")
       , ("load <fname>", "Load a previously-saved game state from the file at <fname>.")
       ]
  where
    prettyCmdHelp :: (String, String) -> String
    prettyCmdHelp (cmd, help) = concat ["\t:", cmd, "\t", " "] ++ help

-- | Print all relevant information regarding the next move.
-- Includes the current board state, and a prompt for a Player to
-- make the next move.
printMoveInfo :: GameState -- current game state
              -> IO () -- IO action that performs the print to STDOUT
printMoveInfo st = do
  printBoard st
  printCurrentPlayer st

-- | Print a message indicating which player must make the next move.
printCurrentPlayer :: GameState -- current game state
                   -> IO () -- IO action that performs the print to STDOUT
printCurrentPlayer st = do
  putStr $ show $ currentPlayer st
  putStrLn " make the next move..."

-- | Print the board in its current state.
printBoard :: GameState -- current game state
           -> IO () -- IO action that performs the print to STDOUT
printBoard st = do
  putStr "\n"
  let b = map (map squareToSymbol) (board st)
  let bStyle = map (intersperse "|") b -- insert dividers between squares
  let divider = intercalate "" $ replicate 33 "-" -- generate dividers between rows
  putStrLn $ unlines $ intersperse divider $ map unwords bStyle

-- | Convert string coordinates in algebraic notation into
-- indices appropriate for the board data structure.
parseCoord :: String -- string coordinate
           -> (Int, Int) -- index coordinate in format (row index, column index)
parseCoord (col:row) = (rowIndex, colIndex)
  where
    rowIndex = 8 - ((read row :: Int) - 1)
    colIndex = (fromEnum col) - 97

-- | Move a piece on the board from one square to another.
-- Return a `TaflError` if the move is invalid or not currently possible.
makeMove :: GameState -- current game state
         -> String -- string coordinate of piece to be moved
         -> String -- string coordinate of destination of move
         -> Either TaflError GameState -- game state after the move
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

-- | Remove all captured pieces from the board.
removeCaptures :: GameState -- current game state
               -> (Int, Int) -- position of just-moved piece that could trigger captures
               -> GameState -- game state after checking for captured pieces
removeCaptures st (x, y) =
  -- check for a both types of capture in every direction
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
-- Else, return GameState unchanged.
custodialCapture :: (Int, Int) -- location of piece to be consumed
                 -> (Int, Int) -- location of supporting piece
                 -> GameState -- current game state
                 -> GameState -- game state after checking for capture
custodialCapture (a, b) (x, y) st =
  if isCustodialCapturePossible st (a, b) (x, y)
  then newSt
  else st
  where
    newSt = setSquare st (a, b) Empty

-- | Perform a fortified lambda capture if the correct conditions are met.
-- Else, return GameState unchanged.
fortifiedLambdaCapture :: (Int, Int) -- location of piece to be consumed
                       -> GameState -- current game state
                       -> GameState -- game state after checking for capture
fortifiedLambdaCapture (a, b) st =
  if isFortifiedLambdaCapturePossible st (a, b)
  then newSt
  else st
  where
    newSt = setSquare st (a, b) Empty

-- | Check if the GameState results in a win or draw.
-- If the end of the game has been reached, reset and return the GameState.
-- Else, return the GameState unmodified.
checkEndGame :: GameState -- current game state
             -> IO (Either TaflError GameState) -- game state after checking for ended game
checkEndGame st = do
  let winner = getWinner st
  case winner of
    Just player -> do
      putStrLn $ (show player) ++ " Win"
      initGameState Nothing (inTestMode st) -- reset game state
    Nothing -> do
      if canPlayerMove st
        then pure $ Right st
        else do
          putStrLn "Draw"
          initGameState Nothing (inTestMode st) -- reset game state

-- | Process a user given command presented as a String, and update the GameState.
-- Return a TaflError if processed command results in an error.
processCommandStr :: GameState -- current game state
                  -> String -- command string
                  -> IO (Either TaflError GameState) -- game state after processing command
processCommandStr st str =
  case commandFromString str of
    Left err   -> pure (Left err)
    Right cmd' -> processCommand st cmd'

-- | Print an Error to STDOUT.
printError :: TaflError -> IO ()
printError (MalformedCommand) = do
  putStrLn "The entered command was malformed"
printError (UnknownCommand) = do
  putStrLn "The entered command was not recognised"
printError (InvalidMove) = do
  putStrLn "Invalid Move!"
printError (CurrentlyUnusableCommand) = do
  putStrLn "The command cannot be used"
printError (CannotSave) = do
  putStrLn "Cannot save game"
printError (CannotLoad) = do
  putStrLn "Cannot load saved game state"
printError (MalformedGameState) = do
  putStrLn "Malformed Game State"
