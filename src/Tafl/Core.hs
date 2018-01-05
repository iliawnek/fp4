{- |
This module defines several core data structures used by the game.
-}
module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , defaultGameState
  , initGameState
  , commandFromString
  , help_text
  , printBoard
  ) where

import System.Exit
import Data.List

data Square = Object
            | Lambda
            | Guard
            | Empty

starting_board :: [[Square]]
starting_board = [[Empty, Empty, Empty, Object, Object, Object, Empty, Empty, Empty]
                 ,[Empty, Empty, Empty, Empty, Object, Empty, Empty, Empty, Empty]
                 ,[Empty, Empty, Empty, Empty, Guard, Empty, Empty, Empty, Empty]
                 ,[Object, Empty, Empty, Empty, Guard, Empty, Empty, Empty, Object]
                 ,[Object, Object, Guard, Guard, Lambda, Guard, Guard, Object, Object]
                 ,[Object, Empty, Empty, Empty, Guard, Empty, Empty, Empty, Object]
                 ,[Empty, Empty, Empty, Empty, Guard, Empty, Empty, Empty, Empty]
                 ,[Empty, Empty, Empty, Empty, Object, Empty, Empty, Empty, Empty]
                 ,[Empty, Empty, Empty, Object, Object, Object, Empty, Empty, Empty]]

-- | The core game state that captures the state of the board, and
-- whether we are playing a game or not.
--
-- You will need to extend this to present the board.
data GameState = GameState
  { inGame     :: Bool
  , inTestMode :: Bool
  , board      :: [[Square]]
  }

defaultGameState :: GameState
defaultGameState = GameState False False starting_board

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ GameState False b starting_board
initGameState (Just f) b = pure $ Left NotYetImplemented

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = InvalidCommand String
               | UnknownCommand
               | NotYetImplemented

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"] -> Right Help
    ["exit"] -> Right Exit
    ["start"] -> Right Start
    ["stop"]  -> Right Stop

    -- You need to specify how to recognise the remaining commands and their arguments here.

    _         -> Left UnknownCommand

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

printSquare :: Square -> String
printSquare Empty = " "
printSquare Object = "O"
printSquare Lambda = "L"
printSquare Guard = "G"

printBoard :: GameState -> IO ()
printBoard st = do
  putStrLn $ unlines [unwords [printSquare ((board st !! y) !! x) | x <- [0..8]] | y <- [0..8]]
