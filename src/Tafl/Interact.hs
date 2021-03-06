{- |

This module defines REPL, the game's mode of interaction.

-}
module Tafl.Interact
  ( repl
  ) where

import Control.Monad

import System.Exit
import System.IO

import Tafl.Core
import Tafl.Process


-- | Core REPL for processing user actions.
repl :: GameState -- ^ The initial starting start of the game
     -> IO ()
repl st = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  LineBuffering
    doREPL st

  where
    doREPL :: GameState -> IO ()
    doREPL st = do
      when (not (inTestMode st) && (inGame st)) $ printMoveInfo st
      putStr "tafl> "
      raw_cmd <- getLine
      when (inTestMode st) $ putStr "\n"
      result  <- processCommandStr st raw_cmd
      case result of
        (Left err) -> do
          printError err
          repl st

        (Right newSt) -> repl newSt
