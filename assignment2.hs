--Group Members: Jiayun Yan, Zhixuan Li

--Exercise 1 
--(a) 
data Cmd = Pen Mode 
		   | Moveto Pos Pos
			| Def String Pars Cmd 
			| Call String Vals 
			| Seq [Cmd]
			deriving Show
data Mode = Up 
			 | Down
			 deriving Show
data Pos = I Int 
			|S String
			deriving Show
type Pars = [String] 
type Vals = [Int]

--(b)
vector = Def "vector" ["x1","y1","x2","y2"] (Seq[Pen Up, Moveto (S "x1")(S "y1"), Pen Down, Moveto (S "x2")(S "y2"), Pen Up])

--(c)
steps :: Int -> Cmd
steps 0 = Seq[Pen Up, Moveto (I 0)(I 0), Pen Down]
steps n = Seq[steps(n-1),Seq[Moveto (I (n-1))(I n)],Seq[Moveto (I n)(I n)]]


--Exercise 2
--(a)
data Circuit = A Gates Links
data Gates = B [(Int, GateFn)]
data GateFn = And
				|Or
				|Xor
				|Not
data Links = C [((Int, Int),(Int,Int))]

instance Show Links where
         show (C (((a,b),(c,d)):xs)) = "from "++show(a)++"."++show(b)++" to "++show(c)++"."++show(d)++"\n"++show(C xs)
         show (C []) = ""

instance Show Gates where
         show (B ((n, gate):ys)) = show(n)++": "++show(gate)++"\n"++show(B ys)
         show (B []) = ""

instance Show GateFn where
         show And = "And"
         show Or = "Or"
         show Xor = "Xor"
         show Not = "Not"
--(b)
adderAbove :: Circuit
adderAbove = A (B [(1, Xor), (2, And)]) (C [((1,1),(2,1)), ((1,2),(2,2))])

--(c)
prettyPrinter :: Circuit -> IO()
prettyPrinter (A a b) = putStrLn("Gates :\n"++show(a)++"Links:\n"++show(b))
