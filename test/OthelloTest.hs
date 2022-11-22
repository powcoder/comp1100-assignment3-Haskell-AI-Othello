https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Main where

import Testing

-- | The list of tests to run. When you define additional test groups,
-- you must list them here or they will not be checked.
--
-- Note that the type of 'Test' has changed from Assignment 2: it is
-- now possible to collect tests into groups, if you write so many
-- tests that you feel that the additional structure would be useful.
allTests :: Test
allTests = TestGroup "allTests"
  []

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the tree of tests that we defined above.
main :: IO ()
main = runTests allTests
