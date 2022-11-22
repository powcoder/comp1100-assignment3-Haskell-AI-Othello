https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-|
Module      : Game
Description : The module for implementing the game logic of Othello
Copyright   : (c) The Australian National University, 2018-2019
License     : AllRightsReserved
-}
module Game where

import Data.Maybe (isJust)
import GameState

-- | It is either a player's turn, or the game is over.
data Turn = Turn Player | GameOver deriving (Eq, Ord, Show)

-- | A `Game` contains the current `Board` and the player who's turn it
-- is (`Nothing` if the game is over)
data Game = Game Turn Board
  deriving (Eq, Ord, Show)

-- | Whose turn is it?
turn :: Game -> Turn
turn (Game t _) = t

-- | Print the game in a human-readable format. It won't work as desired
-- directly inside of GHCi. To use it inside GHCi, try
-- > putStrLn (ppGame yourGame)
ppGame :: Game -> String
ppGame (Game currentTurn board) = unlines [ppBoard board, scoreLine]
  where
    scoreLine = unlines [ ppTurn currentTurn
                        , "Dark:\t" ++ show darkScore
                        , "Light:\t" ++ show lightScore
                        ]
    lightScore = currentScore board Light
    darkScore = currentScore board Dark
    ppTurn (Turn Light) = "Turn:\tLight (o)"
    ppTurn (Turn Dark) = "Turn:\tDark (x)"
    ppTurn GameOver = case compare lightScore darkScore of
      LT -> "Dark wins!"
      EQ -> "Draw"
      GT -> "Light wins!"

-- | The initial board for Othello.
initialBoard :: Board
initialBoard = map makeRow [0..7]
  where
    makeRow :: Int -> [Maybe Player]
    makeRow y = map (\x -> pieceOn (x, y)) [0..7]

    pieceOn :: Position -> Maybe Player
    pieceOn (3, 3) = Just Light
    pieceOn (4, 3) = Just Dark
    pieceOn (3, 4) = Just Dark
    pieceOn (4, 4) = Just Light
    pieceOn _ = Nothing

-- | The initial game state for Othello.
initialGame :: Game
initialGame = Game (Turn Dark) initialBoard

-- | Calculate the current score for a player on a board.
currentScore :: Board -> Player -> Int
currentScore board piece = length [p | Just p <- concat board, p == piece]

-- | Check if a move is legal for a player on a board.
--
-- A move is legal if and only if the position is not already occupied
-- and if placing a stone captures at least one stone from the other
-- player.
legalMove :: Board -> Player -> Position -> Bool
legalMove board player pos
  | outOfBounds pos = boundsError pos "legalMove"
  | isJust (pieceAt board pos) = False
  | otherwise = currentScore board player + 1 <
      currentScore (playMove board player pos) player

-- | Play a move as the player given. It does not check if the move is legal.
playMove :: Board -> Player -> Position -> Board
playMove board player pos
  | outOfBounds pos = boundsError pos "playMove"
  | otherwise = changeTo player (pos:changedPieces) board
  where
    changedPieces = concatMap
      -- Step once so we're not on our starting square
      (\step -> collectPieces (step pos) board isOpponent isUs step dropList)
      directions

    -- The eight different directions, as step functions
    directions = [ \(x, y) -> (x - 1, y - 1)
                 , \(x, y) -> (x - 1, y)
                 , \(x, y) -> (x - 1, y + 1)
                 , \(x, y) -> (x, y - 1)
                 , \(x, y) -> (x, y)
                 , \(x, y) -> (x, y + 1)
                 , \(x, y) -> (x + 1, y - 1)
                 , \(x, y) -> (x + 1, y)
                 , \(x, y) -> (x + 1, y + 1)
                 ]

    -- Collect up any opponent pieces
    isOpponent (Just p) = player == opponent p
    isOpponent _ = False

    -- Stop when we see another of our pieces
    isUs (Just p) = player == p
    isUs _ = False

    -- If we walk off the edge, collect nothing
    dropList _ = []

-- | Make a move as the current player in the game. Returns `Nothing`
-- if the move was an illegal move.
play :: Game -> Position -> Maybe Game
play (Game GameOver _) _ = Nothing
play (Game (Turn player) board) pos
  | legalMove board player pos = Just (Game nextPlayer newBoard)
  | otherwise = Nothing
  where
    newBoard = playMove board player pos

    -- If after this move, the opponent has at least one legal move
    -- then it's their turn. Otherwise, if the current player has a
    -- legal move, it stays their turn. If no-one has a legal move
    -- the game is over.
    nextPlayer
      | any (legalMove newBoard (opponent player)) allPositions
        = Turn (opponent player)
      | any (legalMove newBoard player) allPositions
        = Turn player
      | otherwise = GameOver
