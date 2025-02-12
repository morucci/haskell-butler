module LoggerTest where

import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

import Butler.Prelude
import Butler.Logger
import Butler.Clock

test_logger :: TestTree
test_logger = testCase "Log buffer" do
  logger <- atomically $ newLogger 4
  clock <- newClock
  now <- getTime clock
  traverse_ (atomically . addEvent logger now EventInfo) ([5..9] :: [Int])
  xs <- map (.body) <$> atomically (readLogs logger)
  traverse_ (atomically . addEvent logger now EventInfo) ([0..9] :: [Int])
  ys <- map (.body) <$> atomically (readLogs logger)
  xs `shouldBe` ys
