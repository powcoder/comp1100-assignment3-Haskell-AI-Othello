https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-|
Module      : AI
Description : The AI for Othello
Copyright   : (c) The Australian National University, 2018-2019
License     : AllRightsReserved
-}
module AI where

import Game
import GameState

-- | The type of AI functions. Do not change this.
--
-- The test program will repeatedly call an AI function with
-- increasing lookahead values until it takes too long to generate a
-- result, and use the final result it returns as the "best" move your
-- AI could find.
type AI
  = Game -- ^ The current state of the game
  -> Int -- ^ How far you should look ahead
  -> Position -- ^ The move to make.

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [ ("default", firstLegalMove)
      ]

-- Equivalently: firstLegalMove :: Game -> Int -> (Int, Int)
-- firstLegalMove makes the first legal move that it can find.
firstLegalMove :: AI
firstLegalMove (Game (Turn me) board) _
  = head (filter (legalMove board me) allPositions)
firstLegalMove (Game GameOver _) _
  = error "firstLegalMove: Called on finished game"
