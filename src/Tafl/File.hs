{- |

The `File` module implements the saving and restoring of game state using CSV.

-}
module Tafl.File
  ( saveGameState
  , loadGameState
  ) where

import Data.List
import Data.List.Split

import Tafl.Core
import Tafl.Logic

-- TODO: no message if write fails
saveGameState :: GameState -> String -> IO ()
saveGameState st fname = do
  -- required for writing X for empty castle
  let st' = if getSquare st (4,4) == Empty then setSquare st (4,4) EmptyCastle else st
  -- generate CSV string
  let turn = if (currentPlayer st') == Lambdas then "G to play" else "O to play"
  let csvList = [[turn]] ++ map (\row -> map squareToSymbol row) (board st') ++ [[""]]
  let csvString = intercalate "\n" (map (intercalate ",") csvList)
  -- write CSV string to file
  writeFile fname csvString
  putStrLn $ "State saved in " ++ fname

-- TODO: no message if read fails
loadGameState :: GameState -> String -> IO (Either TaflError GameState)
loadGameState st fname = do
  csvString <- readFile fname
  let csvSt = map (splitOn ",") (endBy "\n" csvString)
  if validateLoadedGameState csvSt
    then do
      let turn = (csvSt !! 0) !! 0
      let newPlayer = if turn == "G to play" then Lambdas else Objects
      let newBoard = map (map symbolToSquare) [csvSt !! i | i <- [1..9]]
      let newSt = st {currentPlayer = newPlayer, board = newBoard}
      pure $ Right newSt
    else pure $ Left MalformedGameState
