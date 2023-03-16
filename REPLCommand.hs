
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

quit :: Parser REPLCommand
quit = do
    symbol ":q" <|> symbol ":quit"
    return Quit

load :: Parser REPLCommand    
load = do
    symbol ":l" <|> symbol ":load"
    filename <- many anychar
    return $ Load filename

eval :: Parser REPLCommand
eval = Eval <$> many anychar

replCommand :: Parser REPLCommand
replCommand = quit <|> load <|> eval  

