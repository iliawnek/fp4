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
  ) where

import System.Exit
import Data.List

data Square = Object
            | Lambda
            | Guard
            | Empty
            deriving (Show, Eq)

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
            deriving Show

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
               | IllegalMove
               | NotYetImplemented

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop
             | Move String String

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"]  -> Right Help
    ["exit"]  -> Right Exit
    ["start"] -> Right Start
    ["stop"]  -> Right Stop
    ["move", src, dst]  -> Right (Move src dst)

    ("help":_)  -> Left MalformedCommand
    ("exit":_)  -> Left MalformedCommand
    ("start":_)  -> Left MalformedCommand
    ("stop":_)  -> Left MalformedCommand
    ("move":_)  -> Left MalformedCommand

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
