https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-# LANGUAGE BangPatterns, LambdaCase #-}
{-|
Module      : Dragons.Timeout
Description : Take the last value produced within a given time
Copyright   : (c) The Australian National University, 2018-2019
License     : AllRightsReserved

Take the last element of a lazily generated list generated within a given
time.
-}

module Dragons.Timeout (USecs, iterateTimeout) where

import Data.IORef
import System.Timeout

type USecs = Int

-- To avoid dependencies, we roll a small version of NFData here

-- | A type which can be evaluated to normal form
class NF a where
  toNF :: a -> ()

instance NF () where
  toNF () = ()

instance NF Int where
  toNF !_ = ()

instance (NF a, NF b) => NF (a,b) where
  toNF (a,b) = toNF a `seq` toNF b

-- | Call the provided function with values drawn from [1..] until the
-- timeout is reached. Return the value that corresponded to the
-- largest argument.
iterateTimeout :: NF a => Double -> (Int -> a) -> IO (Maybe a)
iterateTimeout time f = do
  bestResult <- newIORef Nothing
  let
    timeLimit = round (time * 1000000)
    loop depth = do
      let result = f depth
      toNF result `seq` writeIORef bestResult (Just result)
      loop (depth + 1)
  _ <- timeout timeLimit (loop 1)
  readIORef bestResult
