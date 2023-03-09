
import Control.Applicative
import Data.Char

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

--- | Acceptă orice caracter
anychar :: Parser Char
anychar = satisfy(const True)

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy(\x -> x == c)
char' c  = satisfy ( ==c)

--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | 
        (a, resta) <- apply pa input, 
        (b, restb) <- apply (k a) resta])

cifraIntreParanteze :: Parser Int
cifraIntreParanteze = do
    char '('
    d <- digit
    char ')'
    return (digitToInt d)

cifraSemn :: Parser Int
cifraSemn = do
    s <- satisfy (\c -> c `elem` ['+','-'])
    d <- digit
    return (digitToInt d * (if s == '-' then -1 else 1))    
