module Parser.Lexer where

data Token = Digit Int | Plus | Minus | Mul | Div

lex :: String -> [Token]
lex =
  where lDigit = 
