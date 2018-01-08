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
  ) where

import System.Exit
import Data.List

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
                 ,[Empty,  Empty,  Empty,  Object, Object, Object, Empty,  Empty,  Empty]]

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

-- TODO: when is this used?
defaultGameState :: GameState
defaultGameState = GameState False False starting_board Objects

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ GameState False b starting_board Objects
initGameState (Just f) b = pure $ Left NotYetImplemented

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = MalformedCommand
               | UnknownCommand
               | CurrentlyUnusableCommand
               | InvalidMove
               | CannotSave
               | CannotLoad
               | MalformedGameState
               | NotYetImplemented

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
    ("start":_)  -> Left MalformedCommand
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
