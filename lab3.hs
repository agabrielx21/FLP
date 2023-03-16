import Control.Applicative
import Data.Char
import Distribution.Compat.CharParsing (space)
import Control.Monad (void)

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

convert :: Char -> Int -> Int
convert '+' d = d
convert '-' d = -d

cifraSemn' :: Parser Int
cifraSemn' = pure convert <*> satisfy (\x -> x `elem` ['+','-']) <*> (digitToInt <$> digit)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)
    
-- apply (string "Ma") "Marire"
-- 
-- cazul general -->
-- string(x:xs) = pure (:) <*> char x <*> string xs

--Tema : restul exercitiilor din document
---TEMA : 

instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

-- | Zero or more.
-- many :: Alternative f => f a -> f [a]
-- many v = some_v <|> pure []

-- | One or more.
-- some :: Alternative f => f a -> f [a]
-- some v = pure (:) <*> v <*> many v

naiveNatural :: Parser Int
naiveNatural = do
    digits <- some digit
    return (read digits)

whiteSpace :: Parser ()
whiteSpace = do
    many (satisfy isSpace)
    return ()

-- apply whiteSpace " \t\nksdw"
-- [((),"ksdw"),((),"\nksdw"),((),"\t\nksdw"),(()," \t\nksdw")]
-- apply whiteSpace "ionel"
-- [(), "ionel]

nat :: Parser Int 
nat = do
    digits <- some digit
    return (read digits)

-- apply nat "12ab"
-- [(12, "ab"), (1, "2ab")]
-- ghci> apply nat "ionel"
-- []

lexeme :: Parser a -> Parser a
lexeme = (<* whiteSpace)

-- apply (lexeme (string "Hello")) "Hello World!"
-- [("Hello","World!"),("Hello"," World!"),("Hello"," World!")]

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat     

symbol :: String -> Parser String
symbol =    lexeme . string

-- apply (symbol "if") "if (x) ..."
-- [("if","(x) ..."),("if"," (x) ...")]

reserved :: String -> Parser ()
reserved = void . symbol
-- apply (reserved "if") "if (x) ..."
-- [((),"(x) ..."),(()," (x) ...")] 

comma :: Parser ()
comma = reserved ","

-- apply comma " ,"

parens :: Parser a -> Parser a
parens p = do
    symbol "("
    x <- p
    symbol ")"
    return x 

-- apply (parens (string "Hello")) "(Hello)"

brackets :: Parser a -> Parser a
brackets p = do
  char '[' 
  whiteSpace
  x <- p
  whiteSpace
  char ']'
  return x

-- apply (brackets (string "Hello")) "[ Hello ]"

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = do
  x <- p
  xs <- many (comma >> p)
  return (x:xs)

-- apply (commaSep1 natural) "3 , 4 , a, 5"
-- [([3,4],", a, 5"),([3,4]," , a, 5"),([3,4]," , a, 5"),([3],", 4 ,
-- a, 5"),([3]," , 4 , a, 5"),([3]," , 4 , a, 5")]

commaSep :: Parser a -> Parser [a]
commaSep p = do
  x <- commaSep1 p <|> pure []
  return x

ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = do
  start <- lexeme identStart
  rest <- many $ lexeme identLetter
  return (start : rest)

identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

-- apply (identifier (satisfy isAlpha) (satisfy isAlphaNum)) "ij1
-- + 3"
-- [("ij1","+ 3"),("ij1"," + 3"),("ij","1 + 3"),("i","j1 + 3")]