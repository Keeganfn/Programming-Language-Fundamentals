import HW1types

---EXERCISE 1

---Question 1(a) inserts
ins :: Eq a => a -> Bag a -> Bag a
ins b [] = [(b,1)]
ins b (x:xs) | fst x == b = (fst x, snd x+1):xs
             | otherwise = x : ins b xs


---Helper function to find the given number and subtract by one for 1(b)
finddel :: Eq a => a -> (a, Int) -> (a, Int)
finddel b x  | b == fst x = (fst x, snd x-1)  
             | otherwise = x

---Helper function for 1(b) that removes any pair that has 0 or less as the second value
remzero :: Bag a -> Bag a
remzero [] = []
remzero (x:xs) | snd x <= 0 = remzero xs
               | otherwise = x : remzero xs

---Question 1(b) deletes a given number from multiset
del :: Eq a => a -> Bag a -> Bag a
del b xs = remzero (map (finddel b) xs) 

---Question 1(c) converts list to multiset representation
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)  


---Helper function for 1(d) compares two pairs to see if it is equal and has fewer elements 
findpair :: Eq a => (a, Int) -> (a, Int) -> Bool
findpair b x  | fst b == fst x = if snd b <= snd x then True else False
              | otherwise = False


---Question 1(d) determines if the subbag is present 
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] ys = True
subbag xs [] = False
subbag (x:xs) ys | filter (==True) [(findpair x b) | b <- ys] /= [] = subbag xs ys
                 | otherwise = False


---Question 1(e) tests if Bag is a true set or not
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet (x:xs) | snd x /= 1 = False
             | otherwise = isSet xs


---Question 1(f) gets the size of a bag
size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs


---EXERCISE 2

---Question 2(a) gets the list of nodes from the graph
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm((fst x):[] ++ (snd x):[] ++ nodes xs)


---Question 2(b) gets the list of successors for a given node
suc :: Node -> Graph -> [Node]
suc b [] = []
suc b (x:xs) | b == fst x = (snd x):[] ++ suc b xs
             | otherwise = suc b xs


---Question 2(c) removes a node and all of its incident edges
detach :: Node -> Graph -> Graph
detach b [] = []
detach b (x:xs) | b == fst x || b == snd x = detach b xs
              | otherwise = x:detach b xs


---Question 2(d) creates a cycle of the given number
cyc :: Int -> Graph 
cyc 0 = []
cyc b = [if x < b then (x,x+1) else (x,1) | x <- [1..b]]


---EXERCISE 3

---Question 3(a) gets width of the shape
width :: Shape -> Length
width (Pt x) = 0 
width (Circle x l) = 2*l 
width (Rect x l w) = l 

---Question 3(b) gets bounding box of shape
bbox :: Shape -> BBox
bbox (Pt x) = (x, x) 
bbox (Circle x l) = ((fst x - l, snd x - l), (fst x + l, snd x + l))
bbox (Rect x l w) = (x, (fst x + l, snd x + w))

---Question 3(c) gets minimum x coordinate of shape
minX :: Shape -> Number
minX (Pt x) = fst x
minX (Circle x l) = fst x - l 
minX (Rect x l w) = fst x 


---Helper function for 3(d) to add two points together
addPt :: Point -> Point -> Point
addPt x y = (fst x + fst y, snd x + snd y)

---Question 3(d) moves the origin of the shape by a vector
move :: Shape -> Point -> Shape
move (Pt x) p = Pt (addPt x p)  
move (Circle x l) p = Circle (addPt x p) l 
move (Rect x l w) p = Rect (addPt x p) l w 

























