{- 
  Module : Labs
  Description: Solutions for haskell laboratory tasks.
-}



module Labs where 

-- |Simple list of list conacatenation function.
conact :: [[a]] -> [a]
conact (x:xs) =
  x ++ (concat xs)
conact [] =
  []

getElem :: [a] -> Int -> a
getElem (x:xs) ind 
  | ind == 1 = x
  | ind /= 1 && ind <= length (x:xs) = getElem xs (ind-1)

qsort :: Ord a => [a] -> [a]
qsort [] = 
  []
qsort (x:xs) = 
  qsort smaller ++ x : qsort larger
  where 
    smaller = [y | y <- xs, y <= x]
    larger = [y | y <- xs, y > x]

polygonArea :: [(Float, Float)] -> Float
polygonArea ps = 
  let 
  x = map fst ps
  x' = tail x ++ [head x] 
  y = map snd ps
  y' = tail y ++ [head y]
  xy' = zipWith (*) x y'
  x'y = zipWith (*) x' y
  in 0.5 * (abs (sum xy' - sum x'y))

apply :: [f -> a] -> f -> [a]
apply [] x = []
apply (f:fs) x = f x : apply fs x

any' :: (a -> Bool) -> [a] -> Bool
any' pred [] = 
  False
any' pred (x:xs) 
  | pred x == False = any' pred xs
  | pred x == True = True

curry' f = 
  \x y -> f (x, y)

remove' x [] = 
  [] 
remove' x (y:ys) 
  | x == y = ys
  | otherwise = y : remove' x ys

perms [] =
  [[]]
perms xs = 
  [x : p | x <- xs, p <- perms (remove' x xs)]
type Pos = (Int, Int)

data Move = 
  North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = 
  p
moves (m:ms) p =
  moves ms $ move m p 

data Shape =
  Circle Float 
  | Rect Float Float
  deriving Show

area :: Shape -> Float
area (Circle r) = 
  pi * r * r
area (Rect x y) =
  x * y

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = 
  Nothing
safeDiv a b =
  Just $ div a b
safeHead :: [a] -> Either a String
safeHead [] = 
  Right "Empty list"
safeHead xs = 
  Left $ head xs

data Nat =
  Zero 
  | Succ Nat
  deriving(Show)

fromJust (Just x) = x

toNat :: Int -> Maybe Nat
toNat val 
  | val < 0 = Nothing
  | val == 0 = Just Zero
  | otherwise = Just (Succ(fromJust (toNat (val - 1))))

data Tree a =
  Empty 
  | Node a (Tree a) (Tree a)
  deriving(Show)

depthTree :: Tree a -> Int
depthTree Empty = 0
depthTree (Node _ leftTree rightTree) = 
  1 + max (depthTree leftTree) (depthTree rightTree)

occurs :: Eq a => a -> Tree a -> Bool
occurs _ Empty = False
occurs val (Node nval left right) = 
  val == nval || occurs val left || occurs val right

insertBinaryTree :: Ord a => a -> Tree a -> Tree a
insertBinaryTree val Empty = Node val Empty Empty
insertBinaryTree val (Node nval left right) 
  | val == nval = Node val left right
  | val < nval = Node nval (insertBinaryTree val left) right
  | otherwise = Node nval left (insertBinaryTree val right)

data Tree' a =
  Empty'
  | Node' a [Tree' a]
  deriving(Show)

data Treex a =
  Emptyx
  | Nodex a (Treex a) (Treex a)
  deriving(Show)

symmTreex :: Treex a -> Treex a
symmTreex Emptyx = Emptyx
symmTreex (Nodex v l r) =
  Nodex v (symmTreex r) (symmTreex l)

















