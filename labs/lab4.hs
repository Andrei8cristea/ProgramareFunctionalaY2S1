import Data.Monoid (Any)
{-
#######################################################
EXERCITIUL 1
[x^2 |x <- [1..10], x `rem` 3 == 2]
=> [4 , 25, 64]


[(x,y) | x <- [1..5], y <- [x..(x+2)]]
=>[(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(3,3),(3,4),(3,5),(4,4),(4,5),(4,6),(5,5),(5,6),(5,7)]


[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
=>[(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2)...(3,9)]
=>[(x,x^2)]


[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
=>"FMI"


[[x..y] | x <- [1..5], y <- [1..5], x < y]
=>[[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]


-}

-- #######################################################
-- EXERCITIUL 2
factori :: Int -> [Int]
factori x = [i | i <- [1..x] , x `rem` i == 0]



-- #######################################################
-- EXERCITIUL 3
prim :: Int -> Bool
prim x = length (factori x) == 2



-- #######################################################
-- EXERCITIUL 4
numerePrime :: Int -> [Int]
numerePrime x = [i | i <- [2..x], prim i]



-- #######################################################
-- EXERCITIUL 5
{-
[(x,y) | x <- [1..5], y <- [1..3]]
=>[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),...]

zip [1..5] [1..3]
=>[(1,1),(2m2),(3,3)]
-}

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 (h1:t1) (h2:t2) (h3:t3)= (h1,h2,h3) : myzip3 t1 t2 t3
myzip3 _ _ _ = []



-- #######################################################
-- EXERCITIUL 6
{-
map (\x -> 2 * x) [1..10]
=>[2,4,6,8,10,12,14,16,18,20]


map (1 `elem`) [[2,3], [1,2]]
=>[False, True]


map (`elem` [2,3]) [1,3,4,5]
=>[False,True,False,False]
-}

firstEl :: [(a, b)] -> [a]
firstEl list = map (\(x,y) -> x) list



-- #######################################################
-- EXERCITIUL 7
sumList :: [[Int]] -> [Int]
sumList list = map (\insideList -> sum insideList) list



-- #######################################################
-- EXERCITIUL 8
prel2 :: [Int] -> [Int]
prel2 list = map (\x-> if even x then x `div` 2 else x * 2) list



-- #######################################################
-- EXERCITIUL 9
f9 :: Char -> [String] -> [String]
f9 c list = filter (\word -> c `elem` word) list
-- f9 'a' ["Ana","are","mere"]
-- =>["Ana","are"]



-- #######################################################
-- EXERCITIUL 10
f10 ::  [Int] -> [Int]
f10 list = map (\x -> x^2) (filter (\x -> odd x) list)
-- f10 [1,2,3,4,5,6]
-- [1,9,25]



-- #######################################################
-- EXERCITIUL 11
f11 :: [Int] -> [Int]
f11 list = map (\x -> x^2) (map fst (filter (\(x,y) -> odd y) (zip list [0..length list -1])))
--folosesc map fst ca sa iau doar primul element din tuplul returnat pt care mai apoi ridic la patrat fiecare element
--  f11 [4,3,2,7,9]
-- [9,49]



-- #######################################################
-- EXERCITIUL 12
f12 :: [String] -> [String]
f12 list = map(\word -> eliminaConsoanele word) list

eliminaConsoanele :: String -> String
eliminaConsoanele word = filter (\char -> char `elem` "aeiouAEIOU") word

--  f12  ["laboratorul", "PrgrAmare", "DEclarativa"]
-- ["aoaou","Aae","Eaaia"]



-- #######################################################
-- EXERCITIUL 13
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = f h : mymap f t
-- mymap (*2) [1,2,3]
-- [2,4,6]

myfilter :: (a -> Bool ) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t)
    |f h  = h : myfilter f t
    |otherwise = myfilter f t
-- myfilter odd [1,2,3,4,5,6]
-- [1,3,5]



-- #######################################################
-- EXERCITIUL EXTRA

step:: Char -> String -> [String]
step c config = [take i config ++ [c] ++ drop  (i+1) config | i <- [0..8], config !! i == ' ']
-- step '0' "XXX   XXX"
-- ["XXX0  XXX","XXX 0 XXX","XXX  0XXX"]

next:: Char -> [String] -> [String]
next c cs = concat (map (step c) cs)
-- next 'X' ["         ","0000 0000"]


win :: Char -> [String] -> [String]
win p cs = filter (\conf_curr -> isWinner p conf_curr ) cs

isWinner :: Char -> String-> Bool
isWinner c config = checkLines winningLines
    where
        checkLines [] = False
        checkLines (h:t) = checkLine h || checkLines t

        checkLine [] = True
        checkLine (i:is) = (config !! i == c) && checkLine is

winningLines :: [[Int]]
winningLines = [
    [0,1,2],[3,4,5],[6,7,8],
    [0,3,6],[1,4,7],[2,5,8],
    [0,4,8],[2,4,6]
    ]





-- ordonataNat :: [Int] -> Bool
-- ordonataNat [] = True
-- ordonataNat [x] = True
-- ordonataNat (x:xs) = undefined
-- ordonata :: [a] -> (a -> a -> Bool) -> Bool
-- ordonata = undefined
