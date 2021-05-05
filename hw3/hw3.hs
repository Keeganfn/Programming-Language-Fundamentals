


---PROBLEM 1

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]

type D = Stack -> Maybe Stack


semCmd :: Cmd -> D
semCmd (LD i) s = Just (i:s)
semCmd (ADD) s = if length s < 2 then Nothing else Just ([sum(take 2 s)] ++ drop 2 s)
semCmd (MULT) s = if length s < 2 then Nothing else Just ([product(take 2 s)] ++ drop 2 s)
semCmd (DUP) s = if length s < 1 then Nothing else Just (take 1 s ++ s)


sem :: Prog -> D
sem [] s = Just s 
sem (x:xs) s = case semCmd x s of
                 Just s -> sem xs s
                 _      -> Nothing 


--PROBLEM 2


data Cmd' = Pen Mode
          | MoveTo Int Int
          | Seq Cmd' Cmd'
          deriving Show

data Mode = Up | Down
          deriving Show

type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]


semS :: Cmd' -> State -> (State, Lines)
semS (Pen Up) (s,x,y) = ((Up, x, y), [])
semS (Pen Down) (s,x,y) = ((Down, x, y), [])
semS (MoveTo x1 y1) (Up,x,y) = ((Up, x1, y1), [])
semS (MoveTo x1 y1) (Down,x,y) = ((Down, x1, y1), [(x,y,x1,y1)])
semS (Seq x1 x2) s = (s, snd(semS x1 s) ++ snd(semS x2 s))

sem' :: Cmd' -> Lines
sem' x = snd(semS x (Up, 0, 0))




