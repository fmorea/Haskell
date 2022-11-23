-- my journey into haskell
module MAIN where
import Prelude 
-- to compile in ghci do:
-- :l namefile.hs
-- then you can use the function defined here

fun :: Int -> Int
fun x = x+1

-- same thing with lambda (anonymus functions)
fun2 :: Int -> Int
fun2 x = (\y -> y+1) x

-- binary function 
-- plus(x,y) {return x + y;}  
plus :: Num a => a -> a -> a
plus = (\x->(\y->x+y)) 

-- try it with fun3 [1 .. 10]
-- we'll se later how to implement fmap
fun3 :: [Int] -> [Int]
fun3 [] = []
fun3 list = (\x -> x+1) <$> list 

data Vettore = Vettore Int Int  deriving Show
data Matrice = Matrice Vettore Vettore  deriving Show

-- *MAIN> Matrice (Vettore 1 2) (Vettore 3 4)
-- Matrice (Vettore 1 2) (Vettore 3 4)

-- Bilist :: [a] -> [a] -> Bilist a
data Bilist a = Bilist{ x :: [a], y ::[a] } 

instance Show a => Show (Bilist a) where
 show (Bilist x y) = "Bilist{" ++ show x ++","++ show y ++"}"

-- test with fun <$> (Bilist [1..10][1..10])
instance Functor Bilist where
 fmap _ (Bilist [] _) = Bilist [] []
 fmap _ (Bilist _ []) = Bilist [] []
 fmap f (Bilist x y) = Bilist (f <$> x) (f <$> y)

-- alternative to (!!) (extract n-th from the lists)
-- that is safe (invalid case --> Nothing) and O(n)
lookUp :: Int -> [a] -> Maybe a
lookUp _ []       = Nothing
lookUp 1 (x : _)  = Just x
lookUp i (_ : xs) = lookUp (i - 1) xs

-- unjust with outofbound control
unjust :: Maybe a -> a
unjust (Just x) = x
unjust Nothing = error "OutOfBound Exception" 

-- let's assemble into one function
-- *MAIN> [1,3,4,5,11,24] .* 4
-- 5
(.*) :: [a] -> Int -> a
(.*) list index = (unjust (lookUp index list)) 

-- take the i-th element from each list
bilist_ref i b = (unjust (lookUp i (x b)) , unjust (lookUp i (y b)))

-- lenght of a list
lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1 + (lenght xs)

-- lists are (0:1:2:...:[])
-- reverse finite lists
mirror :: [a] -> [a]
mirror list = (mirrorHelper []) list

mirrorHelper :: [a] -> [a]-> [a]
mirrorHelper acc [] = acc
mirrorHelper [] (x:xs) = mirrorHelper (x:[]) xs 
mirrorHelper notEmptyAcc (x:xs)  = mirrorHelper (x:notEmptyAcc) xs 

-- reverse and select odd
reverseOdd :: [a] -> [a]-> [a]
reverseOdd (x:y:xs) [] = reverseOdd xs (x:[])
reverseOdd (x:y:xs) notEmptyAcc = reverseOdd xs (x:notEmptyAcc) 
reverseOdd _ acc = acc 

-- reverse and select even
reverseEven :: [a] -> [a]-> [a]
reverseEven (x:y:xs) [] = reverseEven xs (y:[])
reverseEven (x:y:xs) notEmptyAcc = reverseEven xs (y:notEmptyAcc) 
reverseEven _ acc = acc 


-- list of elements in odds positions
oddList :: [a] -> [a]
oddList x = mirror (reverseOdd x [])

-- list of elements in even positions
evenList :: [a] -> [a]
evenList x = mirror (reverseEven x [])

-- al posto di utilizzare reverseOdd e reverseEven potevi
-- utilizzare la funzione ++ delle liste per concatenare!
oddEven :: [a] -> Bilist a
oddEven x = Bilist (oddList x) (evenList x)

inv_oddeven :: Bilist a -> [a]->[a]
-- not needed since assumed an even lenght
--inv_oddeven (Bilist [] (x:xs)) acc = inv_oddeven (Bilist [] xs) (x:acc)
--inv_oddeven (Bilist (x:xs) []) acc = inv_oddeven (Bilist xs []) (x:acc)
inv_oddeven (Bilist (x:xs) (y:ys)) [] = inv_oddeven (Bilist xs ys) (y:x:[])
inv_oddeven (Bilist (x:xs) (y:ys)) acc = inv_oddeven (Bilist xs ys) (y:x:acc)
inv_oddeven (Bilist [] []) acc = mirror acc

-- how concat is implemented?? for now not a problem
-- to "remove one level of nesting" from lists of lists
flat :: Foldable t => t [a] -> [a]
flat = concat

-- pair2list :: (a, b) -> [a,b]
pair2list (a, b) = [a, b]

-- zip :: [a] -> [b] -> [(a, b)]
-- considering that our lists are of the same size
-- inv2 :: Bilist a -> [a]
inv2 b = flat $ map pair2list $ zip (x b) (y b)

-- foldl implementation (not efficient because lazy)
-- f a binary function
-- to test
-- (foldLeft (+) 0) [1..4]
foldLeft :: (t1 -> t2 -> t2) -> t2 -> [t1]  -> t2
foldLeft f acc [] = acc
foldLeft f acc (x:xs) = foldLeft f (f x acc) xs

-- (foldRight (+) 0) [1..4]
foldRight :: (t1 -> t2 -> t2) -> t2 -> [t1]-> t2
foldRight f acc [] = acc
foldRight f acc (x:xs)  = f x (foldRight f acc xs)

unchanged :: [a] -> [a]
unchanged = foldr (:) []

reversed :: [a] -> [a]
reversed = foldl  (flip (:)) []

iterator :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
iterator f = foldr f []

-- my implementation of concat [infix is (++)] 
conCat :: [a] -> [a] -> [a]
conCat list1 list2 = conCatHelper [] list1 list2

conCatHelper acc [] [] = mirror acc
conCatHelper acc (x:xs) list2 = conCatHelper (x:acc) xs list2
conCatHelper acc [] (x:xs) = conCatHelper (x:acc) [] xs


--  myconcat [[1],[2],[3]]
myconcat :: [[a]] -> [a]
myconcat = iterator conCat

-- fare operazioni tra liste "incolonnate"
-- esempio 
-- inColumn (+) [1..5] $ inColumn (+) [4,3,2,1,0] [1,1,1,1,1]
-- [6,6,6,6,6]
inColumn ::(Ord a, Num a) => (a -> b -> c)-> [a] -> [b] -> [c]
inColumn _ [] _ = []
inColumn _ _ [] = []
inColumn f (a:as) (b:bs) = (f a b):(inColumn f as bs)

sumInColumn :: (Ord a, Num a) => [a]->[a]->[a]
sumInColumn = inColumn (+)

maxList :: (Ord a, Num a) => [a] -> (a , Int)
maxList = maxHelper [] 0 0

maxHelper :: (Ord a, Num a) => [a]->Int -> Int -> [a] -> (a, Int)
maxHelper [] _ _ [] = error "invalid input"
maxHelper [acc] indM i [] = (acc, indM)
maxHelper [] 0 0 (x:xs) = maxHelper [x] 1 1 xs
maxHelper [old] indM ind (x:xs) 
 | old >= x = maxHelper [old] indM (ind + 1) xs
 | True = maxHelper [x] (ind + 1) (ind + 1) xs

--  bilist_max $ Bilist [1..5] [2..6]
bilist_max ::  (Ord a, Num a) => Bilist a -> Int
bilist_max b = (\(x,y) ->y) $ maxList $ sumInColumn (x b) (y b)

pack :: Eq a => [a] -> [[a]]
pack list = reverse $ packHelper [] [] list

packHelper :: Eq a => [a] -> [[a]] -> [a] -> [[a]] 
 --termination
packHelper currPack acc [] = currPack : acc    
 --(re)initialization of current analyzed pack
packHelper [] acc (x:xs) = packHelper [x] acc xs
--general case
packHelper currPack@(y:ys) acc list@(x:xs) 
 -- o "accresco" il currentPack
 | x == y = packHelper (x:currPack) acc xs
 -- o ho finito di accrescere il currentPack e accresco acc
 | otherwise = packHelper [] (currPack:acc) list

-- *MAIN> rlencode [1,1,1,1,2,2,2,2,3,3,3,3,3,3]
-- [(1,4),(2,4),(3,6)]
rlencode :: Eq a => [a] -> [(a, Int)]
rlencode l = map (\p -> (head p, length p)) $ pack l


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
 | f x = x : myFilter f xs
 | otherwise = myFilter f xs


-- *MAIN> myZip [1..3] [3..7]
-- [(1,3),(2,4),(3,5)]
-- myZip :: [a]->[b]-> [(a,b)]
myZip l1 l2 = inColumn (\x->(\y->(x, y))) l1 l2 

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem x l = foldl (\acc y -> acc || x == y) False l


-- Binary tree
data BTree a = BNode a (BTree a) (BTree a) | BEmpty 
--leaf constructor
bleaf :: a -> BTree a
bleaf n = BNode n BEmpty BEmpty

myTree = BNode 1 (BNode 2 (bleaf 4) BEmpty) (bleaf 3)

instance Eq a => Eq (BTree a) where
 BEmpty == BEmpty = True
 BNode x1 l1 r1 == BNode x2 l2 r2 = x1 == x2 
                                    && l1 == l2 
                                    && r1 == r2
 _ == _ = False


btmap :: (a -> b) -> (BTree a) -> (BTree b)
btmap _ BEmpty = BEmpty
btmap f (BNode x l r) = BNode (f x) (btmap f l) (btmap f r)


 
instance Show a => Show (BTree a) where
 show BEmpty = ""
 show (BNode x l r) =
  "[" ++ show x
  ++ " " ++ show l
  ++ " " ++ show r ++ "]"


instance Functor BTree where
 fmap = btmap

inFtree n = BNode n child child
 where child = inFtree $ n+1


exmon :: (Monad m, Num r) => m r -> m r -> m r
exmon m1 m2 = do x <- m1
                 y <- m2
                 return (x-y)



data State st a = State (st -> (st, a))

instance Functor (State st) where
    fmap f (State g) = State (\s -> let (s', x) = g s
                                    in  (s', f x))

instance Applicative (State st) where
    pure x = State (\t -> (t, x))
    (State f) <*> (State g) =
        State (\state -> let    (s, f') = f state
                                (s', x) = g s
                                in  (s', f' x))

instance Monad (State state) where
    State f >>= g = State (\olds ->
        let (news, value) = f olds
            State f' = g value
        in f' news)

runStateM :: State state a -> state -> (state, a)
runStateM (State f) st = f st

getState = State (\state -> (state, state))

putState new = State (\_ -> (new, ()))

ex = runStateM
     (do x <- return 5
         return (x+1))
     333

ex_ = runStateM
     (do x <- getState
         return (x+1))
     333

ex__ = runStateM
     (do x <- getState
         putState (x+1)
         x <- getState
         return x)
     333
