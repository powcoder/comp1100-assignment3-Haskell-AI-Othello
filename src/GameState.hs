https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-|
Module      : GameState
Description : The module for handling the game state, and operations over it
Copyright   : (c) The Australian National University, 2018-2019
License     : AllRightsReserved
-}
module GameState where

-- | There are two players, 'Dark' and 'Light'.
data Player = Dark | Light
  deriving (Eq, Ord, Show)

-- | Return the opponent of a given player.
opponent :: Player -> Player
opponent Dark = Light
opponent Light = Dark

-- | The Othello board is represented as a list of list of 'Maybe' 'Player'
-- where 'Nothing' is an empty square. It should contain 8 lists, each
-- containing 8 elements.
type Board = [[Maybe Player]]

-- | A Position is an (x, y) pair of coordinates representing a square
-- on a 'Board'.
type Position = (Int, Int)

-- | A list of every possible position on the board.
--
-- [(0,0), (0,1), (0,2), ... (7,5), (7,6), (7,7)]
allPositions :: [Position]
allPositions = [ (x, y) | x <- [0..7], y <- [0..7]]

-- | It is either a player's turn, or the game is over.
-- | Print the board in a human-readable format. It won't work as desired
-- directly inside of GHCi. To use it, try
-- > putStrLn (ppBoard yourBoard)
ppBoard :: Board -> String
ppBoard board = unlines ([ unwords (show n : map pieceToString row)
                         | (n,row) <- zip [1 :: Int ..] board]
                         ++ [unwords (map return (' ':['a'..'h']))])
  where
    pieceToString Nothing = "."
    pieceToString (Just Dark) = "x"
    pieceToString (Just Light) = "o"

-- | Are the coords given out of bounds?
outOfBounds :: Position -> Bool
outOfBounds (x, y) = not (elem x [0..7] && elem y [0..7])

-- | Throw an error for a position out of bounds, with the coordinate
-- and string given.
boundsError :: Position -> String -> a
boundsError (x, y) callee =
  error (concat [ callee, ": Invalid bounds (x=", show x
                , ") (y=", show y, ")"
                ])

-- | Find the piece at the given position.
pieceAt :: Board -> Position -> Maybe Player
pieceAt board pos@(x, y)
  | outOfBounds pos = boundsError pos "pieceAt"
  | otherwise = case drop y board of
      [] -> error "pieceAt: Malformed Board"
      row:_ -> case drop x row of
        [] -> error "pieceAt: Malformed Board"
        piece:_ -> piece

-- | Replace the piece at the given position with the provied piece.
update :: Position -> Maybe Player -> Board -> Board
update pos@(x, y) piece board
  | outOfBounds pos = boundsError pos "update"
  | otherwise = updateRow y board
  where
    updateRow _ [] = error "update:updateRow: Index error"
    updateRow 0 (row:rows) = updateCol x row:rows
    updateRow n (row:rows) = row : updateRow (n-1) rows
    updateCol _ [] = error "update:updateCol: Index error"
    updateCol 0 (_:cols) = piece:cols
    updateCol n (col:cols) = col : updateCol (n-1) cols

-- | Change the positions given on the board to being owned by the player.
changeTo :: Player -> [Position] -> Board -> Board
changeTo _ [] board = board
changeTo p (pos:poss) board = changeTo p poss (update pos (Just p) board)

-- | Build a list of coords from the board, starting at the position
-- given. Mostly used to collect pieces to change after a move was made.
collectPieces
  :: Position -- ^ Starting position
  -> Board -- ^ Board to walk over
  -> (Maybe Player -> Bool) -- ^ Do we include this cell in the result?
  -> (Maybe Player -> Bool) -- ^ Should we stop?
  -> (Position -> Position) -- ^ Step (return the next position to check)
  -> ([Position] -> [Position])
     -- ^ What to do with collected results, if we walked off the edge
  -> [Position]
collectPieces position board keepTaking stopTaking nextPos edgeFunction =
  go position []
  where
    go pos pieces
      | outOfBounds pos = edgeFunction pieces
      | keepTaking currentPiece = go (nextPos pos) (pos:pieces)
      | stopTaking currentPiece = pieces
      | otherwise = []
      where
        currentPiece = pieceAt board pos
