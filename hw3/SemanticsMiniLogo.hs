module SemanticsMiniLogo where

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



