module LibRpn.Parser.Lexer where
import LearningHaskell.Parsing
import Data.Maybe

data Token = Digit Int | Plus | Minus | Mul | Div
  deriving Eq

instance Show Token where
  show (Digit i) = show i
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"

lex :: Parser String [Token]
lex = mapWith catMaybes $ repeated (choice [lDigit, lPlus, lMinus, lMul, lDiv, lSkipWhitespace])
  where lDigit          = mapWith (pure . Digit) digit
        lPlus           = mapWith (pure . const Plus) (just '+')
        lMinus          = mapWith (pure . const Minus) (just '-')
        lMul            = mapWith (pure . const Mul) (just '*')
        lDiv            = mapWith (pure . const Div) (just '/')
        lSkipWhitespace = ignore whitespace
