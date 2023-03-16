
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
    putStr "miniHaskell> "
    f <- getLine
    case parseFirst replCommand f of
        Nothing -> putStrLn "Cannot parse command" >> main
        Just Quit -> return ()
        Just (Load _) -> putStrLn "Not implemented" >> main
        Just (Eval exp) ->
            case parseFirst exprParser exp of 
                Nothing -> putStrLn "Cannot parse command" >> main
                Just e -> putStrLn (showExp e) >> main

    


