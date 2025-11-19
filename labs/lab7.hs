data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)
           
instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"           

-- #######################################################
-- 1

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

-- #######################################################
-- 2

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

-- #######################################################
-- 3

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare

-- #######################################################
-- 4

lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' k (BNode left key val right)
    | k < key = lookup' k left
    | k > key = lookup' k right
    | otherwise = val 

tree1 = BNode (BNode Empty 1 (Just "unu") Empty)
              2 (Just "doi")
              (BNode Empty 3 Nothing Empty)

test41 = lookup' 1 tree1 == Just "unu"

-- #######################################################
-- 5

keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode left key _ right) = keys left ++ [key] ++ keys right
-- extrag cheile din subarborele stang
-- apoi cheia curenta
-- apoi subarborele drept
-- voi returna astfel lista cheilor in ordine deoarece
-- sunt intr un BST
test51 = keys tree1 == [1,2,3]

-- #######################################################
-- 6

values :: IntSearchTree value -> [value]
values Empty = []
values (BNode left _ val right) = values left ++ maybeToList val  ++ values right
    where
        maybeToList Nothing = []
        maybeToList (Just x) = [x]

test61 = values tree1 == ["unu", "doi"]

-- #######################################################
-- 7

insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert k v Empty = BNode Empty k (Just v) Empty
insert k v (BNode left key val right)
    | k < key = BNode (insert k v left) key val right
    | k > key = BNode left key val (insert k v right)
    | otherwise = BNode left key (Just v) right

tree2 = insert 3 "trei" tree1

test71 = values tree2 == ["unu", "doi", "trei"]

-- #######################################################
-- 8

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete _ Empty = Empty
delete k (BNode left key val right)
    | k < key = BNode (delete k left) key val right
    | k > key = BNode left key val (delete k right)
    | otherwise = BNode left key Nothing right

tree3 = delete 3 tree2

test81 = values tree3 == ["unu", "doi"]

-- #######################################################
-- 9

toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode left key val right) = toList left ++ maybeToList (fmap (\v -> (key, v)) val) ++ toList right
    where 
        maybeToList Nothing = []
        maybeToList (Just x) = [x]
-- folosesc fmap deoarce ma raportez la elemente ale clasei functor (just maybe)
test91 = toList tree1 == [(1,"unu"), (2,"doi")]

-- #######################################################
-- 10

fromList :: [(Int,value)] -> IntSearchTree value 
fromList = foldr (\(k,v) rest -> insert k v rest) Empty

pairs = [(10,"zece"), (5,"cinci"), (15,"cincisprezece"),
         (3,"trei"), (7,"sapte"), (12,"doisprezece"), (18,"optsprezece"),
         (1,"unu"), (4,"patru"), (6,"sase"), (8,"opt")]

tree4 = fromList pairs

test101 = keys tree4 == [1,3,4,5,6,7,8,10,12,15,18]
test102 = values tree4 == ["unu","trei","patru","cinci","sase","sapte","opt","zece","doisprezece","cincisprezece","optsprezece"]


-- #######################################################
-- 11

printTree :: IntSearchTree value -> String
printTree Empty = ""
printTree (BNode left key _ right) =
    (case left of
        Empty -> ""
        _     -> "(" ++ printTree left ++ ") ")
    ++ show key ++
    (case right of
        Empty -> ""
        _     -> " (" ++ printTree right ++ ")")

test111 = printTree tree3 == "(1) 2 (3)"

-- #######################################################
-- 12

buildBalanced :: [(Int, value)] -> IntSearchTree value
buildBalanced [] = Empty
buildBalanced xs =
    let mid = length xs `div` 2
        (leftList, (k,v):rightList) = splitAt mid xs
    in BNode (buildBalanced leftList) k (Just v) (buildBalanced rightList)

balance :: IntSearchTree value -> IntSearchTree value
balance tree = buildBalanced (toList tree)

-- calculez pozitia de mijloc
-- folosesc splitAt sa separ lista in doua jumatati
-- creez un nod BNode cu cheia k si valoarea just v 

-- practic iau elementul din mijloc ca radacina si construiesc recursiv
-- in stanfa si in dreapta rezultatul fiind un arbore echilibrat



-- testare:
tree5 = fromList pairs
treeBalanced :: IntSearchTree String
treeBalanced = balance tree5

test121 = keys treeBalanced == [1,3,4,5,6,7,8,10,12,15,18]
test122 = values treeBalanced == ["unu","trei","patru","cinci","sase","sapte","opt","zece","doisprezece","cincisprezece","optsprezece"]
test123 = toList treeBalanced == toList tree4


