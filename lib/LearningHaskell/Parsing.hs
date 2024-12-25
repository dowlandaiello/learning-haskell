module LearningHaskell.Parsing where

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

-- Convert a parsed element into another element
map :: (b -> c) -> Parser a b -> Parser a c
map = (.) . fmap

-- Parse multiple occurrences of an element
repeated :: Int -> Parser a b -> Parser [a] [b]
repeated n p = sequence . Prelude.map p . take n

-- Parse one element after another
andThen :: Parser a b -> Parser a c -> Parser [a] (b, c)
andThen pA pB = \elems -> case elems of
  (a:b:_) -> liftA2 (,) (pA a) (pB b)
  allE -> Left $ "found " ++ show (length allE) ++ "; expected at least 2 elems"

-- Tries one parser on an input or another on failure
or :: Parser a b -> Parser a b -> Parser a b
or pA pB = \e -> either (const $ pB e) pure (pA e)
