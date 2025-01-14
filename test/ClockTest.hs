module ClockTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Prelude
import Butler.Clock

test_time :: TestTree
test_time = testCase "Time increase" do
  clock <- newClock
  sleep 10
  Milli t <- from <$> clock.getTime
  assertBool ("Elapsed time is out range: " <> show t) (t >= 10 && t < 11)
