module MemoryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Memory
import Butler.Storage
import Butler.Prelude

data TestMem = TestMem {buf :: ByteString} deriving (Eq, Show)

test_memory :: TestTree
test_memory = testCase "Memory" do
    storage <- newStorage ".butler-tmp"
    (v, mv) <- newMemoryVar storage "test" (pure True)
    v @?= True

    -- read memory
    rm <- atomically $ readMemoryVar mv
    rm @?= True

    -- todo: check storage
