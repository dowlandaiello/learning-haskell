 module Main where

import qualified LibRpl.Parser.Lexer as L
import qualified LibRpl.Parser.Parser as P
import System.Exit (exitFailure)
import Test.HUnit
import Data.Either
import Data.Char

-- Lexer tests
csL :: [(String, [L.Token])]
csL =  [("3 + 4", [L.Digit 3, L.Plus, L.Digit 4])]

testLexer :: Test
testLexer =  TestCase $ assertEqual "Can lex all inputs" (all id [L.lex c == Right e | (c, e) <- csL]) True

-- Parser tests
testParseNums :: Test
testParseNums = TestCase $ assertEqual "Can parse numbers" ((L.lex "123456") >>= P.parseNum) (Right $ P.Num 123456)

-- Suite
tests :: Test
tests =  TestList [TestLabel "Lexer should work" testLexer, TestLabel "Should be able to parse numbers" testParseNums]

main :: IO Counts
main =  runTestTT tests
