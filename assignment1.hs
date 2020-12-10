import Data.List (nub,sort)

norm :: Ord a => [a] -> [a]
norm = sort . nub

--Group Members: Jiayun Yan, Zhixuan Li

--Exercise 1 
--(a) 
type Bag a = [(a,Int)] 
	
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,n):zs) | x==y      = (y,n+1):zs
				 	  | otherwise = (y,n):ins x zs

--(b)
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):zs) | x==y && n>1      = (y,n-1):zs
				     | x==y             = zs
					  | otherwise        = (y,n):del x zs

--(c)
bag :: Eq a => [a] -> Bag a
bag = foldr ins []

--(d)
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((x,n):xs) ((y,m):ys) | x==y && n<=m  = True
								     | x==y && n>m   = False
									  | otherwise     = subbag ((x,n):xs) ys        

--(e)
isBag :: Eq a => Bag a -> Bag a -> Bag a
isBag [] _ = []
isBag ((x,n):xs) ((y,m):ys) | x==y && n<=m   = (x,n):isBag xs ys
								    | otherwise      = isBag ((x,n):xs) ys

--(f)
size :: Bag a -> Int
size [] = 0
size ((x,n):xs) = n + size xs


--Exercise 2
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

g :: Graph 
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph 
h = [(1,2),(1,3),(2,1),(3,2),(4,4)] 

--(a)
rmdups :: Eq a => [a] -> [a]
rmdups [] = [] 
rmdups (x:xs) | x `elem` xs = rmdups xs
              | otherwise   = x:rmdups xs	
nodes1 :: Graph -> [Node]
nodes1 [] = [] 
nodes1 ((x,y):xs) | x==y      = x:nodes1 xs
				      | otherwise = x:y:nodes1 xs
nodes = rmdups.nodes1

--(b)
suc :: Node -> Graph -> [Node]
suc _ [] = []
suc x ((a,b):as) | x==a      = b:suc x as
				 	  | otherwise = suc x as

--(c)
detach :: Node -> Graph -> Graph
detach _ [] = [] 
detach a ((x,y):xs) | a==x || a==y = detach a xs
						  | otherwise    = (x,y):detach a xs

--(d)
cyc :: Int -> Graph
cyc 0 = []
cyc x = ((x-1),x):cyc (x-1)


--Exercise 3
type Number = Int
type Point = (Number,Number) 
type Length = Number
data Shape = Pt Point | Circle Point Length | Rect Point Length Length 
			  deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

--(a)
width :: Shape -> Length
width (Pt (_,_))       = 0
width (Circle (_,_) r) = (2*r)
width (Rect (_,_) w _) = w 

--(b)
bbox :: Shape -> BBox
bbox (Pt (a,b))       = ((a,b),(a,b))
bbox (Circle (c,d) r) = ((c-r,d-r), (c+r,d+r))
bbox (Rect (e,f) w h) = ((e,f),(e+w,f+h))

--(c)
minX :: Shape -> Number
minX (Pt (a,b))      = a
minX (Circle (c,d) r) = c-r 
minX (Rect (e,f) w h) = e

--(d)
move :: Shape -> Point -> Shape
move (Pt (a,b)) (c,d)       = (Pt (a+c,b+d)) 
move (Circle (e,f) r) (g,h) = (Circle (e+g,f+h) r)
move (Rect (i,j) m n) (k,l) = (Rect (i+k,j+l) m n)

--(e)
alignLeft :: Figure -> Figure
alignLeft [Pt(a,b),Circle(c,d)r,Rect(e,f)w h]  = [Pt(0,b),Circle(r,d)r,Rect(0,f)w h]

--(f)
inside :: Shape -> Shape -> Bool
inside (Pt (a,b)) (Pt (a1,b1))               | a==a1 && b==b1  = True
                                             | otherwise       = False
inside (Pt (a,b)) (Circle (c1,d1) r1)        | a>=(c1-r1) && a<=(c1+r1) && b>=(d1-r1) && b<=(d1+r1) = True
                                             | otherwise = False
inside (Circle (c,d) r) (Pt(_,_)) = False
inside (Rect (e,f) w h) (Pt(_,_)) = False     
inside (Pt (a,b)) (Rect (e1,f1) w1 h1)       | a>=e1 && a<=(e1+w1) && b>=f1 && b<=(f1+h1) = True
                                             | otherwise                                  = False
inside (Circle (c,d) r) (Circle (c2,d2) r2)  | r<=r2 &&(c-r)>=(c2-r2) && (c-r)<=(c2+r2) && (c+r)<=(c2+r2) && (d-r)>=(d2-r2) && (d-r)<=(d2+r2) && (d+r)<=(d2+r2) = True
															| otherwise = False
inside (Circle (c,d) r) (Rect (e2,f2) w2 h2) | 2*r<=w2 && 2*r <=h2 && (c-r)>=e2 && (c-r)<=(e2+w2) && (c+r)<=(e2+w2) && (d-r)>=f2 && (d-r)<=(f2+h2) && (d+r)<=(f2+h2) = True 
															| otherwise = False
inside (Rect (e,f) w h) (Circle (c3,d3) r3)  | 2*r3<=w && 2*r3 <=h && (c3-r3)>=e && (c3-r3)<=(e+w) && (c3+r3)<=(e+w) && (d3-r3)>=f && (d3-r3)<=(f+h) && (d3+r3)<=(f+h) = True 
															| otherwise = False
inside (Rect (e,f) w h) (Rect (e3,f3) w3 h3) | w<=w3 && h<=h3 && e>=e3 && e<=(e3+w3) && (e+w)<=(e3+w3) && f>=f3 && f<=(f3+h3) && (f+h)<=(f3+h3) = True 
															| otherwise = False
