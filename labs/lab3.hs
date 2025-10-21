import Distribution.Compat.CharParsing (CharParsing(string))
import Data.Char(isDigit, digitToInt)
import Data.List (permutations)
-- #######################################################
-- EXERCITIUL 1

verifL :: [Int] -> Bool
verifL array = if (length array `mod` 2 == 0)
               then True
               else False


verifL2 :: [Int] -> Bool
verifL2 array = even (length array)

--folosesc drop care imi taie exact primele n elemente din lista
takefinal :: [Int] -> Int -> [Int]
takefinal array n
    | n <= length array = drop (length array - n) array
    | otherwise = array

takefinal_string :: [Char] -> Int -> [Char]
takefinal_string array n
    | n <= length array = drop (length array - n) array
    | otherwise = array

remove :: [Int] -> Int -> [Int]
remove array n = take n array ++ drop (n+1) array
    

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t
 
-- #######################################################
-- EXERCITIUL 2

myreplicate :: Int -> Int -> [Int]
myreplicate n v
    | n == 0 = []
    | otherwise =  v : myreplicate(n-1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    |odd h = h + sumImp t
    |otherwise = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | take 1 h == "A" = length h + totalLen t
    | otherwise = totalLen t

-- #######################################################
-- EXERCITIUL 3


-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | reverse h == h = nr_vocale_string h+ nrVocale t
    | otherwise = nrVocale t


nr_vocale_string :: String -> Int
nr_vocale_string "" = 0
nr_vocale_string (h:t)
    | elem h "AEIOUaeiou" == True = 1 + nr_vocale_string t
    | otherwise = nr_vocale_string t


-- #######################################################
-- EXERCITIUL 4


-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

f :: Int -> [Int] -> [Int]
f param [] = []
f param (h:t)
    | even h = h : param : f param t 
    | otherwise = h : f param t

-- #######################################################
-- EXERCITIUL 5


semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]


-- divizori 4 == [1,2,4]
divizori :: Int -> [Int]
divizori param = [ i | i <- [1..param] , param `mod` i == 0]

-- #######################################################
-- EXERCITIUL 6

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (h:t) = divizori h : listadiv t
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]


-- #######################################################
-- EXERCITIUL 7

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t)
    |a <= h && h <=b = h : inIntervalRec a b t
    |otherwise = inIntervalRec a b t



inInterval :: Int -> Int -> [Int] -> [Int] 
inInterval a b list = [i | i <- [a..b], elem i list]

-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]


-- #######################################################
-- EXERCITIUL 8

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1+ pozitive t
    |otherwise = pozitive t

pozitive :: [Int] -> Int
pozitive list = length [i | i <- list, i > 0]

-- pozitive [0,1,-3,-2,8,-1,6] == 3

-- #######################################################
-- EXERCITIUL 9

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec list = helper list 0
    
helper :: [Int] -> Int -> [Int]
helper [] _ = []
helper (h:t)  pos
    |odd h = pos : helper t (pos + 1)
    |otherwise = helper t (pos + 1)

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare list = [ pos| (i, pos) <- zip list [0..(length list - 1)],odd i ]

-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

-- #######################################################
-- EXERCITIUL 10



multDigitsRec ::  String -> Int
multDigitsRec [] = 1
multDigitsRec (h:t) 
    | isDigit h = digitToInt h * multDigitsRec t
    | otherwise = multDigitsRec t

multDigits ::  String -> Int
multDigits string = product [ digitToInt char | char <- string , isDigit char]    

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

-- #######################################################
-- EXERCITIUL 11

--permutations

-- #######################################################
-- EXERCITIUL 12

combinari :: [Int] -> Int -> [[Int]]
combinari _ 0 = [[]]
combinari [] _ = []
combinari (h:t) k
    | k < 0 = []
    | otherwise = [h: xs | xs <- combinari t (k-1)] ++ combinari t k

-- #######################################################
-- EXERCITIUL 13

-- #######################################################
-- EXERCITIUL 14
