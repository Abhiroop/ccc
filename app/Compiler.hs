module Compiler where

-- Hutton's Razor
data Expr = Val Int
          | Add Expr Expr
          deriving Show

-- eval :: Expr -> Int
-- eval (Val n) = n
-- eval (Add e1 e2) = eval e1 + eval e2

-- Adding a stack
type Stack = [Int]

eval' :: Expr -> Stack -> Stack
eval' (Val n) s   = push n s
eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m:n:s) = n + m :s

eval :: Expr -> Int
eval e = head (eval' e [])
