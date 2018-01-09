{- |
This module defines several core data structures used by the game.
-}
module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , Square(..)
  , Player(..)
  , defaultGameState
  , initGameState
  , commandFromString
  , help_text
  , getSquare
  , setSquare
  , switchPlayer
  , squareToSymbol
  , symbolToSquare
  , saveGameState
  , loadGameState
  ) where

import System.Exit
import Data.List
import Data.List.Split
import Control.Exception

data Square = Object
            | Lambda
            | Guard
            | Empty
            | EmptyCastle -- only used for saving to file
            deriving (Show, Eq)

-- | Returns a symbol to represent a square on the board.
squareToSymbol :: Square -> String
squareToSymbol Empty = " "
squareToSymbol Object = "O"
squareToSymbol Lambda = "L"
squareToSymbol Guard = "G"
squareToSymbol EmptyCastle = "X" -- only used for saving to file

-- | Converts a symbol to a square.
symbolToSquare :: String -> Square
symbolToSquare " " = Empty
symbolToSquare "O" = Object
symbolToSquare "L" = Lambda
symbolToSquare "G" = Guard
symbolToSquare "X" = Empty

starting_board :: [[Square]]
starting_board = [[Empty,  Empty,  Empty,  Object, Object, Object, Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Object, Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Empty ]
                 ,[Object, Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Object]
                 ,[Object, Object, Guard,  Guard,  Lambda, Guard,  Guard,  Object, Object]
                 ,[Object, Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Object]
                 ,[Empty,  Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Object, Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Object, Object, Object, Empty,  Empty,  Empty ]]

data Player = Lambdas
            | Objects
            deriving (Show, Eq)

-- | Switch to the other player so that they can make their turn.
switchPlayer :: GameState -> GameState
switchPlayer st = st {currentPlayer = if (currentPlayer st == Lambdas) then Objects else Lambdas}

-- | The core game state that captures the state of the board, and
-- whether we are playing a game or not.
--
-- You will need to extend this to present the board.
data GameState = GameState
  { inGame        :: Bool
  , inTestMode    :: Bool
  , board         :: [[Square]]
  , currentPlayer :: Player
  }

defaultGameState :: Bool -> Bool -> GameState
defaultGameState inGame inTestMode = GameState inGame inTestMode starting_board Objects

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ defaultGameState False b
initGameState (Just f) b = do
  result <- loadGameState (defaultGameState False b) f
  case result of
    Left err -> pure $ Left err
    Right newSt -> pure $ Right newSt

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = MalformedCommand
               | UnknownCommand
               | CurrentlyUnusableCommand
               | InvalidMove
               | CannotSave
               | CannotLoad
               | MalformedGameState

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop
             | Move String String
             | Save String
             | Load String

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"]  -> Right Help
    ["exit"]  -> Right Exit
    ["start"] -> Right Start
    ["stop"]  -> Right Stop
    ["move", src, dst]  -> Right (Move src dst)
    ["save", fname]  -> Right (Save fname)
    ["load", fname]  -> Right (Load fname)

    ("help":_)  -> Left MalformedCommand
    ("exit":_)  -> Left MalformedCommand
    ("start":_) -> Left MalformedCommand
    ("stop":_)  -> Left MalformedCommand
    ("move":_)  -> Left MalformedCommand
    ("save":_)  -> Left MalformedCommand
    ("load":_)  -> Left MalformedCommand

    _ -> Left UnknownCommand

commandFromString _  = Left UnknownCommand


help_text :: String
help_text = unlines $
     [ "Tafl Help text:", ""]
  ++ map prettyCmdHelp
       [ ("help",  "Displays this help text." )
       , ("exit",  "Exits the Command Prompt.")
       , ("start", "Initiates a game."        )
       , ("stop",  "Stops a game."            )
       ]
  where
    prettyCmdHelp :: (String, String) -> String
    prettyCmdHelp (cmd, help) = concat ["\t:", cmd, "\t", " "] ++ help

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

saveGameState :: GameState -> String -> IO (Maybe TaflError)
saveGameState st fname = do
  -- required for writing X for empty castle
  let st' = if getSquare st (4,4) == Empty then setSquare st (4,4) EmptyCastle else st
  -- generate CSV string
  let turn = if (currentPlayer st') == Lambdas then "G to play" else "O to play"
  let csvList = [[turn]] ++ map (\row -> map squareToSymbol row) (board st') ++ [[""]]
  let csvString = intercalate "\n" (map (intercalate ",") csvList)
  -- write CSV string to file
  result <- try (writeFile fname csvString) :: IO (Either SomeException ())
  case result of
    Left _ -> pure $ Just CannotSave
    Right _ -> pure $ Nothing

loadGameState :: GameState -> String -> IO (Either TaflError GameState)
loadGameState st fname = do
  result <- try (readFile fname) :: IO (Either SomeException String)
  case result of
    Left _ -> pure $ Left CannotLoad
    Right csvString -> do
      let csvSt = map (splitOn ",") (endBy "\n" csvString)
      if validateLoadedGameState csvSt
        then do
          putStrLn $ "State loaded from " ++ fname
          let turn = (csvSt !! 0) !! 0
          let newPlayer = if turn == "G to play" then Lambdas else Objects
          let newBoard = map (map symbolToSquare) [csvSt !! i | i <- [1..9]]
          let newSt = st {currentPlayer = newPlayer, board = newBoard, inGame = True}
          pure $ Right newSt
        else pure $ Left MalformedGameState

-- Checks if a loaded game state is valid.
validateLoadedGameState :: [[String]] -> Bool
validateLoadedGameState csvSt =
  (length csvSt) == 10 &&
  (length turnLine) == 1 &&
  (turn == "G to play" || turn == "O to play") &&
  -- all board rows must have exactly 9 values
  foldl (\acc len -> acc && (len == 9)) True (map length boardLines) &&
  -- all board symbols must be valid squares
  foldl (\acc sym -> acc && (validateSymbol sym)) True symbols &&
  -- if there is an X, it must be in the middle
  ((countSymbol "X") == 0 ||
  ((countSymbol "X") == 1 && castleSymbol == "X")) &&
  -- castle symbol must be X or L
  (castleSymbol == "X" || castleSymbol == "L") &&
  -- cannot exceed maximum number of each piece
  (countSymbol "L") <= 1 &&
  (countSymbol "G") <= 8 &&
  (countSymbol "O") <= 16
  where
    turnLine = csvSt !! 0
    turn = turnLine !! 0
    boardLines = [csvSt !! i | i <- [1..9]]
    symbols = foldl (++) [] boardLines
    validateSymbol s =
      s == " " || s == "O" || s == "L" || s == "G" || s == "X"
    countSymbol sym = foldl (\c s -> if s == sym then c + 1 else c) 0 symbols
    castleSymbol = symbols !! 40
