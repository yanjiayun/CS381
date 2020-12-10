--Group members: Jiayun Yan, Zhixuan Li

--Exercise 1
type Stack = [Int]
type Prog = [Cmd]
data Cmd = LD Int 
         | ADD 
         | MULT 
         | DUP
         | INC 
         | SWAP 
         | POP Int
			deriving Show
type Rank = Int 
type CmdRank = (Int,Int)

--(a)
rankC :: Cmd -> CmdRank
rankC (LD _)  = (0,1)
rankC ADD     = (2,1)
rankC MULT    = (2,1)
rankC DUP     = (1,2)
rankC INC     = (1,1)
rankC SWAP    = (2,2)
rankC (POP x) = (x,0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP x  = rank x 0

rank :: Prog -> Rank -> Maybe Rank
rank [] x | x>=0 = Just x 
			 | x<0  = Nothing
rank (a:as) b = if (b-n)>=0 then rank as (b-n+m) else Nothing
				  where (n,m) = rankC a

--(b)
semStatTC :: Prog -> Maybe Stack
semStatTC a | b /= Nothing = Just (sem a)
				| b == Nothing = Nothing
				where b = rankP a 

sem :: Prog -> Stack
sem = foldl (flip semCmd) []

semCmd :: Cmd -> Stack -> Stack         --It can be simplified by define type A before like type A = Stack -> Stack
semCmd (LD i) x  			  = (i:x)
semCmd (ADD) (x1:x2:xs)   = ((x1+x2):xs)
semCmd (MULT)  (x1:x2:xs) = ((x1*x2):xs)
semCmd (DUP)   (x1:xs)    = (x1:x1:xs)
semCmd (INC)   (x1:xs)    = ((x1+1):xs)
semCmd (SWAP)  (x1:x2:xs) = (x2:x1:xs)
semCmd (POP n) xs         = drop n xs


--Exercise 2
--(a)
data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int, Int) 
	
bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD x y) | x1 >= y1 = (x1, (x2+y2))
    			  | x1 < y1  = (y1, (x2+y2)) 
				  where
				  		(x1, x2) = bbox x 
						(y1, y2) = bbox y
bbox (LR x y) | x2 >= y2 = ((x1+y1), x2)
				  | x2 < y2  = ((x1+y1), y2)
   			  where (x1, x2) = bbox x
         			(y1, y2) = bbox y

--(b)
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD x y) = case rect x of         
        				Nothing -> Nothing
        				Just (x1, x2) -> case rect y of 
                         			  Nothing -> Nothing
                         			  Just (y1, y2) -> case (x1 == y1) of
                                          			 True -> Just (x1,x2+y2)
                                          			 False -> Nothing
rect (LR x y) = case rect x of        
        				Nothing -> Nothing
        				Just (x1, x2) -> case rect y of 
                         			  Nothing -> Nothing
                         			  Just (y1, y2) -> case (x2 == y2) of
                                          			 True -> Just (x1+y1,x2)
                                          			 False -> Nothing
