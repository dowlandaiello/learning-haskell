module LearningHaskell.Parsing where

import Data.Char
import Data.Either
import Control.Monad

-- Any function that can combine multiple inputs of one type into an output of
-- another type, possibly failing
type Parser a b = a -> Either Error b

-- An individual parsing error located at a specific span
type Error = String

-- Matches a specific element, and produces some output after parsing
just :: (Eq a, Show a) => a -> Parser a a
just a = \e ->
    let reasonExpected = "found " ++ show e ++ "; expected " ++ show a
     in if e == a then pure a else Left reasonExpected

-- Matches some element that fits a predicate
filter :: (Show a) => (a -> Bool) -> Parser a a
filter = flip filterExpected Nothing

-- Matches some element that fits a predicate
filterExpected :: (Show a) => (a -> Bool) -> Maybe String -> Parser a a
filterExpected f expected = \e -> if f e then pure e else Left $ "predicate match failed: found " ++ show e ++ "; expected " ++ show expected

-- Convert a parsed element into another element
mapWith :: (b -> c) -> Parser a b -> Parser a c
mapWith = (.) . fmap

--- Fallible mapping from one parser to another
tryMapWith :: Parser b c -> Parser a b -> Parser a c
tryMapWith = (<=<)

-- Matches anything in the parser
any :: Parser a a
any = pure

-- Parse multiple occurrences of an element
repeatedN :: Int -> Parser a b -> Parser [a] [b]
repeatedN n p = sequence . Prelude.map p . take n

-- Parse infinite occurrences of an element until the parser fails
repeated :: Parser a b -> Parser [a] [b]
repeated p = sequence . takeWhile isRight . Prelude.map p

-- Parse one element after another
andThen :: Parser a b -> Parser a c -> Parser [a] (b, c)
andThen pA pB = \elems -> case elems of
  (a:b:_) -> liftA2 (,) (pA a) (pB b)
  allE -> Left $ "found " ++ show (length allE) ++ "; expected at least 2 elems"

-- Tries one parser on an input or another on failure
orParse :: Parser a b -> Parser a b -> Parser a b
orParse pA pB = \e -> either (const $ pB e) pure (pA e)

-- Tries any number of possible parsers
choice :: [Parser a b] -> Parser a b
choice parsers = \e -> firstRight [p e | p <- parsers]
  where firstRight x = case x of
          (Right r):_ -> pure r
          (Left l):xs -> either (const $ Left l) pure (firstRight xs)
          [] -> Left "exhausted all parsers"

-- Parser that discards its input, producing an error message
failWith :: String -> Parser a b
failWith msg = const $ Left msg

-- Parses ascii digits
digit :: Parser Char Int
digit = choice parsers
  where parsers = [mapWith (const i) (just $ intToDigit i) | i <- [0..9]]

-- Parses whitespace unicode characters
whitespace :: Parser Char Char
whitespace = filterExpected isSpace $ Just "whitespace"

accepted :: Parser a b -> Parser a (Maybe b)
accepted = mapWith $ pure

-- Parser that discards matched input
ignore :: Parser a b -> Parser a (Maybe c)
ignore = mapWith $ const Nothing
