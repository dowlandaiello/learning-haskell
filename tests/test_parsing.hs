module Main where

import qualified LearningHaskell.Parsing as P
import System.Exit (exitFailure)
import Test.HUnit
import Data.Either
import Data.Char

-- Just tests
testJust :: Test
testJust = TestCase $ assertEqual "Just can produce a matched good input" (P.just 'a' 'a') (Right 'a')

testJustBad :: Test
testJustBad = TestCase $ assertEqual "Just cannot produce a matched bad input" (isLeft $ P.just 'a' 'd') True

-- Map tests
testMap :: Test
testMap = TestCase $ assertEqual
  "Map can produce a parser that converts a matched input into a desired output"
  (P.mapWith (\x -> "bro: " ++ show x) (P.just 'a') 'a') (Right "bro: 'a'")

-- Repeated tests
testRepeated :: Test
testRepeated = TestCase $ assertEqual "Repeated can parse multiple of the same letter, one after the other" (P.repeated 2 (P.just 'a') "aa") (Right "aa")

-- Or tests
testOr :: Test
testOr = TestCase $ assertEqual "Or can recover from failure, parsing an alternative candidate" (P.orParse (P.just 'a') (P.just 'b') 'b') (Right 'b')

-- Digit parsing tests
testDigit :: Test
testDigit = TestCase $ assertEqual "Digit can parse any digit [0, 9]" (all (id) [P.digit (chr i) == pure i | i <- [0..9]]) True

-- Suite
tests :: Test
tests = TestList [TestLabel "Just accepts an input correctly" testJust, TestLabel "Just discards an input correctly" testJustBad, TestLabel "map maps a parsed value to another value" testMap, TestLabel "Repeated can parse multiple inputs correctly" testRepeated, TestLabel "Or can recover from failure" testOr, TestLabel "Digit can parse any digit [0, 9]" testDigit]

main :: IO Counts
main = runTestTT tests
