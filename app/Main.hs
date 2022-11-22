https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-# LANGUAGE Unsafe #-}
{-|
Module      : Main
Description : Main entry point to the Othello game
Copyright   : (c) The Australian National University, 2018-2019
License     : AllRightsReserved
-}
module Main where

import safe AI (AI)
import Config
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad hiding (join)
import Data.Char (ord, toLower)
import Data.Foldable (for_, traverse_)
import Dragons.Timeout
import Dragons.Config
import Dragons.GUI
import Dragons.Network
import Game
import GameState

-- A transcript for a game - recorded backwards
-- i.e last played move at the head of the list
type Transcript = [Position]

-- Convert a transcript to a string in the expected
-- order
ppTranscript :: Transcript -> String
ppTranscript = concatMap (\(x, y) -> [['A'..'H'] !! x, ['1'..'8'] !! y])
             . reverse

main :: IO ()
main = do
  config <- parseGameConfig
  case config of
    HelpConfig message -> putStrLn message
    ConsoleConfig p1 p2 timeout ->
      playConsoleGame p1 p2 timeout initialGame []
    GUIConfig p1 p2 timeout ->
      playGUIGame p1 p2 timeout initialGame []
    NetHostConfig ai timeout port ->
      hostNetGame ai timeout port
    NetClientConfig ai hostname port ->
      joinNetGame ai hostname port
    TournamentConfig{} -> error "Tournament game not yet implemented"

playConsoleGame
  :: Maybe AI
  -> Maybe AI
  -> Double
  -> Game
  -> Transcript
  -> IO ()
playConsoleGame player1 player2 timeout = go
  where
    -- Step the game
    go game transcript = do
      putStrLn (ppGame game)
      case turn game of
        GameOver -> putStrLn $ "Transcript: " ++ ppTranscript transcript
        Turn Dark -> darkMove game transcript >>= uncurry go
        Turn Light -> lightMove game transcript >>= uncurry go
    -- Move for the dark player
    darkMove = case player1 of
      Nothing -> humanMove Dark
      Just ai -> aiMove ai Dark
    -- Move for the light player
    lightMove = case player2 of
      Nothing -> humanMove Light
      Just ai -> aiMove ai Light
    -- Make a move for an AI
    aiMove ai player game transcript = do
      let
        wrappedAI :: Game -> Int -> (Int, Position)
        wrappedAI g d = (d, ai g d)
      move <- iterateTimeout timeout (wrappedAI game)
      case move of
        Nothing -> error (show player ++ " failed to make a move in time")
        Just (lookahead, pos) ->
          case play game pos of
            Nothing -> error (show player ++ " made an invalid move")
            Just game' -> do
              putStrLn (show player ++ " had a lookahead of " ++ show lookahead)
              return (game', pos:transcript)
    -- Make a move for a Human (or more accurately, wait for a move
    -- from a human)
    humanMove player game transcript = do
      putStrLn ("Please make a move (e.g. d3) for " ++ show player)
      move <- getLine
      case map toLower move of
        [c,r] | c `elem` ['a'..'h'],
                r `elem` ['1'..'8'],
                x <- ord c - ord 'a',
                y <- ord r - ord '1',
                Just game' <- play game (x, y) ->
                  return (game', (x, y):transcript)
        _ -> do
          putStrLn (ppGame game)
          -- Print "Invalid move" after reprinting the board to
          -- make it clearer something has gone wrong
          putStrLn ("Invalid move (" ++ move ++ ")")
          humanMove player game transcript

playGUIGame
  :: Maybe AI
  -> Maybe AI
  -> Double
  -> Game
  -> Transcript
  -> IO ()
playGUIGame player1 player2 timeout g t = do
  putStrLn "See the game at http://127.0.0.1:3000"
  runGUI $ \moves toDraw ->
    -- Visually crash
    (`catch` (\e -> do atomically $ traverse_ (writeTChan toDraw)
                                          [DrawTable, Fail (show e)]
                       throwIO (e :: SomeException))) $ do
      atomically $ traverse_ (writeTChan toDraw) (drawGame g)
      go moves toDraw g t
  where
    -- Step the game
    go moves toDraw game@(Game _ board) transcript = do
      atomically $ traverse_ (writeTChan toDraw)
        [ DrawTurn (turn game)
        , DrawScores (currentScore board Dark) (currentScore board Light)]
      case turn game of
        GameOver -> putStrLn $ "Transcript: " ++ ppTranscript transcript
        Turn Dark -> darkMove >>= uncurry (go moves toDraw)
        Turn Light -> lightMove >>= uncurry (go moves toDraw)
    -- Move for the dark player
      where
        darkMove = case player1 of
          Nothing -> humanMove Dark
          Just ai -> aiMove ai Dark
        -- Move for the light player
        lightMove = case player2 of
          Nothing -> humanMove Light
          Just ai -> aiMove ai Light
        -- Make a move for an AI
        aiMove ai player = do
          move <- iterateTimeout timeout (ai game)
          case move of
            Nothing -> error (show player ++ " failed to make a move in time")
            Just pos ->
              case play game pos of
                Nothing -> error (show player ++ " made an invalid move")
                Just game' -> do
                  animateMove player pos game game' toDraw
                  atomically $ writeTChan toDraw (DrawPiece player pos)
                  return (game', pos:transcript)
        -- Make a move for a Human (or more accurately, wait for a move
        -- from a human)
        humanMove player = do
          atomically $ flushChan moves -- Clear any moves in the queue
          pos <- atomically $ readTChan moves
          case play game pos of
            Nothing -> do
              atomically $ traverse_ (writeTChan toDraw) [Save, BadMove pos]
              threadDelay 500000
              atomically $ writeTChan toDraw Restore
              humanMove player
            Just game' -> do
              animateMove player pos game game' toDraw
              return (game', pos:transcript)


animateMove
  :: Player
  -> Position
  -> Game
  -> Game
  -> TChan GUIAction
  -> IO ()
animateMove player pos (Game _ board) (Game _ board') toDraw = do
  atomically $ writeTChan toDraw (DrawPiece player pos)

  let diffs = [(x, y) | (y, ps) <- zip [0 ..]
                                   (zipWith zip board board')
                     , (x, (p1,p2)) <- zip [0..] ps, p1 /= p2
                     , (x, y) /= pos]
  for_ diffs $ \newPos -> do
    threadDelay 200000
    atomically $ writeTChan toDraw (DrawPiece player newPos)


flushChan :: TChan a -> STM ()
flushChan chan = do
  next <- tryReadTChan chan
  case next of
    Nothing -> return ()
    Just _ -> flushChan chan

hostNetGame :: AI -> Double -> Integer -> IO ()
hostNetGame ai timeout port = do
  putStrLn $ "Ready for network game. If in the CSIT labs,"
        ++ "the hostname is 192.168.roomNumber.computerNumber\n"
        ++ "you can find it by running \"ip a | grep 192.168\" in the "
        ++ "terminal and looking for the numbers before /24."
  host port (start initialGame [])
  where
    start game transcript recv send end = do
      message <- atomically $ readTChan recv
      case message of
        JoinGame -> do
          atomically $ writeTChan send (StartGame timeout)
          ourMove ai game transcript timeout recv send end
        _ -> error $ "hostNetGame:start: Bad message " ++ show message

joinNetGame :: AI -> String -> Integer -> IO ()
joinNetGame ai hostname port = join hostname port start
  where
    start recv send end = do
      atomically (writeTChan send JoinGame)
      message <- atomically $ readTChan recv
      case message of
        StartGame timeout -> theirMove ai initialGame [] timeout recv send end
        _ -> do
          putStrLn $ "joinNetGame:start : Bad message " ++ show message
          end

endGame
  :: Game
  -> Transcript
  -> TChan NetMessage
  -> TChan NetMessage
  -> IO ()
  -> IO ()
endGame game transcript recv send end = do
  putStrLn (ppGame game)
  atomically $ writeTChan send Finished
  Just Finished <- readTChanTimeout 1 recv
  putStrLn $ "Transcript: " ++ ppTranscript transcript
  end

ourMove
  :: AI
  -> Game
  -> Transcript
  -> Double
  -> TChan NetMessage
  -> TChan NetMessage
  -> IO ()
  -> IO ()
ourMove _ g@(Game GameOver _) transcript _ recv send end =
  endGame g transcript recv send end
ourMove ai game transcript timeout recv send end = do
  putStrLn (ppGame game)
  let
    wrappedAI :: Game -> Int -> (Int, Position)
    wrappedAI g d = (d, ai g d)
  move <- iterateTimeout timeout (wrappedAI game)
  case move of
    Nothing -> putStrLn "We failed to make a move in time" >> end
    Just (lookahead, pos) ->
      case play game pos of
        Nothing -> putStrLn "We made an invalid move" >> end
        Just game' -> do
          putStrLn ("We had a lookahead of " ++ show lookahead)
          atomically $ writeTChan send (MakeMove (NetGame game') pos)
          (if turn game == turn game' then ourMove else theirMove) ai
            game' (pos:transcript) timeout recv send end


theirMove
  :: AI
  -> Game
  -> Transcript
  -> Double
  -> TChan NetMessage
  -> TChan NetMessage
  -> IO ()
  -> IO ()
theirMove _ g@(Game GameOver _) transcript _ recv send end =
  endGame g transcript recv send end
theirMove ai game transcript timeout recv send end = do
  putStrLn (ppGame game)
  move <- readTChanTimeout (timeout+0.2) recv
  case move of
    Nothing -> do
      putStrLn "They failed to make a move in time"
      atomically $ writeTChan send (Disconnect "You took too long")
      end
    Just (MakeMove (NetGame game') pos)
      | Just game' /= play game pos -> do
          putStrLn "They made an illegal move"
          atomically $ writeTChan send (Disconnect "They made an illegal move")
          end
      | turn game == turn game' -> theirMove ai game' (pos:transcript)
                                     timeout recv send end
      | otherwise -> ourMove ai game' (pos:transcript)
                       timeout recv send end
    Just mess -> putStrLn ("Unexpected message: " ++ show mess) >> end
  pure ()

readTChanTimeout :: Double -> TChan a -> IO (Maybe a)
readTChanTimeout timeout chan = do
  delay <- registerDelay (round $ timeout * 1000000)
  atomically $
        Just <$> readTChan chan
    <|> Nothing <$ (check <=< readTVar) delay
