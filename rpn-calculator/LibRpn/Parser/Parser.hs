module LibRpn.Parser.Parser where
import qualified LearningHaskell.Parsing as P
import qualified LibRpn.Parser.Lexer as L

data Expr = Num Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving Eq

instance Show Expr where
  show (Num i) = show i
  show (Add a b) = show a ++ " + " ++ show b
  show (Sub a b) = show a ++ " - " ++ show b
  show (Mul a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b

parse :: P.Parser [L.Token] Expr
parse = P.choice [parseNum]

parseNum :: P.Parser [L.Token] Expr
parseNum = P.mapWith (Num . fromDigits) $ P.repeated (P.tryMapWith parseDigit P.any)
  where fromDigits     = foldl addDigit 0
        addDigit num d = 10 * num + d

parseDigit :: P.Parser L.Token Int
parseDigit (L.Digit n) = pure n
parseDigit _           = Left "not a valid digit"


