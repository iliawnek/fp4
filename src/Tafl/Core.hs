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

-- | A square on the board.
-- May or may not contain a piece.
data Square = Object
            | Lambda
            | Guard
            | Empty
            | EmptyCastle -- only used for saving to file
            deriving (Show, Eq)

-- | Returns a character symbol to represent a square on the board.
squareToSymbol :: Square -- square to return a symbol for
               -> String -- symbol
squareToSymbol Empty = " "
squareToSymbol Object = "O"
squareToSymbol Lambda = "L"
squareToSymbol Guard = "G"
squareToSymbol EmptyCastle = "X" -- only used for saving to file

-- | Converts a character symbol to a square.
-- Used when loading game state from file.
symbolToSquare :: String -- string to return a Square instance for
               -> Square
symbolToSquare " " = Empty
symbolToSquare "O" = Object
symbolToSquare "L" = Lambda
symbolToSquare "G" = Guard
symbolToSquare "X" = Empty

-- | Represents one of the two players playing the game.
data Player = Lambdas
            | Objects
            deriving (Show, Eq)

-- | Switch to the other player so that they can make their turn.
switchPlayer :: GameState -- current game state
             -> GameState -- game state after switching players
switchPlayer st = st {currentPlayer = if (currentPlayer st == Lambdas) then Objects else Lambdas}

-- | The core game state that captures:
-- * whether we are playing a game or not
-- * whether we are in testing mode or not (testing mode disables display of game state between moves)
-- * the current state of the board
-- * whose turn it is
data GameState = GameState
  { inGame        :: Bool
  , inTestMode    :: Bool
  , board         :: [[Square]]
  , currentPlayer :: Player
  }

-- | Starting configuration of the board for new games.
startingBoard :: [[Square]]
startingBoard = [[Empty,  Empty,  Empty,  Object, Object, Object, Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Object, Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Empty ]
                 ,[Object, Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Object]
                 ,[Object, Object, Guard,  Guard,  Lambda, Guard,  Guard,  Object, Object]
                 ,[Object, Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Object]
                 ,[Empty,  Empty,  Empty,  Empty,  Guard,  Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Empty,  Object, Empty,  Empty,  Empty,  Empty ]
                 ,[Empty,  Empty,  Empty,  Object, Object, Object, Empty,  Empty,  Empty ]]

-- | Create a default starting game state (according to game rules).
defaultGameState :: Bool -- should testing mode be enabled?
                 -> GameState
defaultGameState inTestMode = GameState False inTestMode startingBoard Objects

-- | Initialise a new game state.
-- Returns a TaflError if the state cannot be loaded from file.
initGameState :: Maybe FilePath -- path to file containing state, if supplied
              -> Bool -- should testing mode be enabled?
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ defaultGameState b
initGameState (Just f) b = do
  result <- loadGameState (defaultGameState b) f
  case result of
    Left err -> pure $ Left err
    Right newSt -> pure $ Right newSt

-- | All errors encountered by the game.
data TaflError = MalformedCommand
               | UnknownCommand
               | CurrentlyUnusableCommand
               | InvalidMove
               | CannotSave
               | CannotLoad
               | MalformedGameState

-- | All supported REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop
             | Move String String
             | Save String
             | Load String

-- | Try to construct a command from the given string.
-- Return a TaflError if the string does not represent a supported command.
commandFromString :: String -- command string
                  -> Either TaflError Command
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

-- | Get the Square at a specific location on the board.
getSquare :: GameState -- current game state
          -> (Int, Int) -- coordinates of the square to be retrieved
          -> Square
getSquare st (a, b) = ((board st) !! a) !! b

-- | Change a Square on the board.
setSquare :: GameState -- current game state
          -> (Int, Int) -- coordinates of the square to be changed
          -> Square -- new Square value
          -> GameState
setSquare st (a, b) replacement = st {board = newBoard}
  where
    -- generate new board where only square at (a, b) has been changed
    replaceSquare = \(sq, i) -> if (i == b) then replacement else sq
    replaceRow = \(row, i) -> if (i == a) then (map replaceSquare (zip row [0..])) else row
    newBoard = map replaceRow (zip (board st) [0..])

-- | Save the current game state to a file.
-- Return TaflError if given path cannot be saved to.
saveGameState :: GameState -- current game state
              -> String -- path to file
              -> IO (Maybe TaflError)
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

-- | Load a game state from file, then return it.
-- Return TaflError if file at given path cannot be read.
-- Return TaflError if game state in file is malformed.
loadGameState :: GameState -- current game state
              -> String -- path to file
              -> IO (Either TaflError GameState)
loadGameState st fname = do
  result <- try (readFile fname) :: IO (Either SomeException String)
  case result of
    Left _ -> pure $ Left CannotLoad
    Right csvString -> do
      -- convert CSV to list of lists
      let csvSt = map (splitOn ",") (endBy "\n" csvString)
      if validateLoadedGameState csvSt
        then do
          -- generate new game state if it is valid
          putStrLn $ "State loaded from " ++ fname
          let turn = (csvSt !! 0) !! 0
          let newPlayer = if turn == "G to play" then Lambdas else Objects
          let newBoard = map (map symbolToSquare) [csvSt !! i | i <- [1..9]]
          let newSt = st {currentPlayer = newPlayer, board = newBoard, inGame = True}
          pure $ Right newSt
        else pure $ Left MalformedGameState

-- | Return True if a loaded game state is valid, according to the game's rules.
validateLoadedGameState :: [[String]] -- loaded and parsed game state
                        -> Bool
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
