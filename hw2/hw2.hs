
--PROBLEM 1
data Mode = Up | Down

data Pos = I Int | S String

data Pars = P String Pars | G String

data Vals = V Int Vals | K Int

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def String (Pars) Cmd
         | Call String (Vals)
         | C Cmd Cmd


vector = Def ("vector") (P "x1" (P "y1" (P "x2" (G "y2"))))  
         (C (Pen Up) (C (Moveto (S "x1",S "y1")) (C (Pen Down) (Moveto (S "x2",S "y1")))))

steps :: Int -> Cmd
steps 0 = (Pen Up)
steps x = C (Pen Up) (C (Moveto (I x, I x)) (C (Pen Down) (C (Moveto (I (x-1), I x)) (C (Moveto (I (x-1), I (x-1))) (steps (x-1))))))


--PROBLEM 2

data RegEx = Epsilon
           | Dot  
           | Cin Char
           | Question RegEx
           | Star RegEx
           | Plus RegEx
           | Seq RegEx RegEx
           | Either RegEx RegEx
           | Par [RegEx]

accept :: RegEx -> String -> Bool
accept (Seq e1 e2) s = or [accept e1 v && accept e2 w | (v,w) <- splits s]
accept (Question e1) s = accept e1 s 
--accept (Star e1) s = accept (Plus (e1)) s
--accept (Plus e1) s = accept (Seq ((e1) (Star(e1)))) s


--Plus( Cin ("s") ) "sssssss"

--accept (Question e1) s | e1 == [] = accept Epsilon (Empty)
--                     | otherwise = accept Cin (e1) e1
--                      | otherwise = accept Cin (e1) e1


splits :: [a] -> [([a],[a])]
splits [] = []
splits [x] = [([],[x]),([x],[])]
splits (x:xs) = [([],x:xs)] ++ [(x:s,t) | (s,t) <- splits xs]





