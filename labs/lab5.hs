-- #######################################################
-- EXERCITIUL 1

sqodd :: [Int] -> Int
sqodd = foldr (+) 0 . map(^2) . filter odd

-- #######################################################
-- EXERCITIUL 2

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- #######################################################
-- EXERCITIUL 3

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies function = foldr (\x rest-> function x && rest) True

--allVerifies even [2, 4, 6]


-- #######################################################
-- EXERCITIUL 4

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies function = foldr (\x rest-> function x || rest) False

--anyVerifies even [1,3,5]

-- #######################################################
-- EXERCITIUL 5

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x : acc else acc) []

-- #######################################################
-- EXERCITIUL 6

listToInt :: [Integer] -> Integer
listToInt = foldl (\rest x -> rest * 10 + x) 0

--listToInt [1,2,0,5]

-- #######################################################
-- EXERCITIUL 7

-- A

rmChar :: Char -> String -> String
rmChar c = foldl (\rest x -> if x /= c then rest ++ [x] else rest) ""

-- rmChar 'a' "banana"

-- B

rmCharsRec :: String -> String -> String
rmCharsRec chars str= foldr rmChar str chars

--rmCharsRec ['a'..'l'] "halloween"

-- C

--exact ca la b

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

-- #######################################################
-- EXERCITIUL 8

myReverse :: [Int] -> [Int]
myReverse = foldl (\rest x -> x : rest) []

--myReverse [1,2,3]

-- #######################################################
-- EXERCITIUL 9

myElem :: Int -> [Int] -> Bool
myElem e= foldl (\rest x -> rest || x == e) False

--myElem 4 [1,3,4,8]

-- #######################################################
-- EXERCITIUL 10

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldl (\(restx, resty) (x,y) -> (restx ++ [x], resty ++ [y])) ([],[])

-- myUnzip [(1,'a'), (2,'b'), (3,'c')]

-- #######################################################
-- EXERCITIUL 11

myUnion :: [Int] -> [Int] -> [Int]
myUnion l1 l2= foldl (\rest x -> if x `elem` rest then rest else rest ++ [x]) l1 l2

--myUnion [1,2,3] [2,3,4]

-- #######################################################
-- EXERCITIUL 12

myIntersect :: [Int] -> [Int] -> [Int]
myIntersect l1 l2 = foldl (\acc x -> if x `elem` l1 && not (x `elem` acc) then acc ++ [x] else acc) [] l2
--myIntersect [1,2,3] [2,3,4]

-- #######################################################
-- EXERCITIUL 13

myPermutations :: [Int] -> [[Int]]
myPermutations = foldl (\rest x -> concatMap (insertEverywhere x) rest) [[]]

-- folosesc concatMap = map pt fiecarea element din lista si 
-- concateneaza listele rezultate
insertEverywhere :: Int -> [Int] -> [[Int]]
insertEverywhere x [] = [[x]]
insertEverywhere x (h:t) = (x:h:t) : map (h:) (insertEverywhere x t)

-- insertEverywhere 2 [1,3]
-- [[2,1,3],[1,2,3],[1,3,2]]


