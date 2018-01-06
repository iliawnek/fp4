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
  , printMoveInfo
  , makeMove -- TODO: remove?
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
parseCoordinateString (column:row) = (rowIndex, columnIndex)
  where
    rowIndex = 8 - ((fromEnum column) - 97)
    columnIndex = (read row :: Int) - 1

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
makeMove :: GameState -> String -> String -> GameState
makeMove st src dst = newSt
  where
    (iSrcRow, iSrcCol) = parseCoordinateString src
    (iDstRow, iDstCol) = parseCoordinateString dst
    srcSquare = getSquare st (iSrcRow, iSrcCol)
    dstSquare = getSquare st (iDstRow, iDstCol)
    newSt = if (dstSquare == Empty)
      then setSquare (setSquare st (iSrcRow, iSrcCol) Empty) (iDstRow, iDstCol) srcSquare
      else st
