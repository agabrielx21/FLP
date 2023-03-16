
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar = getVar

showExp :: ComplexExp -> String
showExp (CX x) = showVar x
showExp (Nat n) = show n
showExp (CLam x y) = "(\\" ++ showVar x ++ " -> " ++ showExp y ++ ")"
showExp (CApp x y) = "(" ++ showExp x ++ " -> " ++ showExp y ++ ")"
showExp (Let x y z) = "let " ++ showVar x ++ ":= " ++ showExp y ++ "in" ++ showExp z ++ ")"
showExp (LetRec x y z) = "letrec " ++ showVar x ++ ":= " ++ showExp y ++ "in" ++ showExp z ++ ")"
showExp (List es) = "[" ++ intercalate "," (map showExp es) ++ "]"  


