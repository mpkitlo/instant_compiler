module Common where

data Instruction = 
    PrintInt Int    |
    Add Int Int Int |
    Sub Int Int Int |
    Mul Int Int Int |
    Div Int Int Int |
    Store Int Integer

-- xd = putStr