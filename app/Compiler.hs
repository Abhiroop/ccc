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

-- eval' :: Expr -> Stack -> Stack
-- eval' (Val n) s   = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m:n:s) = n + m :s

eval :: Expr -> Int
eval e = head (eval' e [])

-- Adding a continuation

type Cont = Stack -> Stack

-- eval'' :: Expr -> Cont -> Cont
-- eval'' e c s = c (eval' e s)

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c s   = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c.add)) s

eval' :: Expr -> Cont
eval' e = eval'' e id
