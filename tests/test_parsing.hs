module Main where

import qualified LearningHaskell.Parsing as P
import System.Exit (exitFailure)
import Test.HUnit
import Data.Either

-- Just tests
testJust :: Test
testJust = TestCase $ assertEqual "Just can produce a matched good input" (P.just 'a' 'a') (Right 'a')

testJustBad :: Test
testJustBad = TestCase $ assertEqual "Just cannot produce a matched bad input" (isLeft $ P.just 'a' 'd') True

-- Map tests
testMap :: Test
testMap = TestCase $ assertEqual
  "Map can produce a parser that converts a matched input into a desired output"
  (P.map (\x -> "bro: " ++ show x) (P.just 'a') 'a') (Right "bro: 'a'")

-- Suite
tests :: Test
tests = TestList [TestLabel "Just accepts and discards an input correctly" testJust, TestLabel "map maps a parsed value to another value" testMap]

main :: IO Counts
main = runTestTT tests
