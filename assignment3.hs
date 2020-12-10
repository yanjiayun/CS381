--Group member: Jiayun Yan, Zhixuan Li

import Data.Maybe

--Exercise 1
type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show
type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

semCmd :: Cmd -> D
semCmd (LD s) stack' = case stack' of
					  Just ss -> Just ([s] ++ ss)
					  _       -> Nothing
semCmd ADD stack' = case stack' of
				  Just (s:x:ss)   -> Just ([s+x] ++ ss)
				  _       -> error ("The stack contains fewer than 2 elements.") 
semCmd MULT stack' = case stack' of
					Just (s:x:ss)   -> Just ([s*x] ++ ss)
					_       -> error ("The stack contains fewer than 2 elements.") 
semCmd DUP stack' = case stack' of
				  Just (s:ss)     -> Just ([s,s] ++ ss)
				  _       -> error ("The stack might be an empty list.") 

sem :: Prog -> D
sem [] ss = ss
sem (p:ps) ss = sem ps (semCmd p ss)


--Exercise 2
data Cmd' = Pen Mode
          | MoveTo Int Int
          | Seq Cmd' Cmd'
          deriving Show
data Mode = Up | Down
          deriving (Show,Eq)
type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd' -> State -> (State,Lines)
semS (Pen p1) (p2, x, y) = if p1 /= p2 then ((p1,x, y), []) else ((p2,x,y), [])
semS (MoveTo x1 y1) s'@(m, x2, y2) = if m == Down then (s', [(x1,y1,x2,y2)]) else (s', [])
semS (Seq a b) s' = (st2,list1++list2)
    where (st1, list1) = semS a s'
          (st2, list2) = semS b st1

sem' :: Cmd' -> Lines
sem' s = snd (semS s (Up,0,0))
