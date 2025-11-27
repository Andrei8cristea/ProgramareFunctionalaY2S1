import Utils.Containers.Internal.BitQueue (toListQ)



class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  toList :: c key value -> [(key, value)]
-- #######################################################
-- 1
  keys :: c key value -> [key]
  keys = map fst . toList
--extrag doar cheile cu map fst

  values :: c key value -> [value]
  values = map snd . toList
--extrag doar valorile cu map snd

  fromList :: Ord key => [(key,value)] -> c key value
  fromList = foldr( uncurry insert ) empty
--pornesc de la lista goala si inserez fiecare pereche de tip
--cheie valoare

test11 = 
    let c = fromList [(1,"a"),(2,"b")]:: PairList Int String
    in keys c == [1,2] && values c == ["a","b"]

-- #######################################################
-- 2
newtype PairList k v = PairList { getPairList :: [(k, v)] }
    deriving Show

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k,v)]

    insert k v (PairList xs) =
        PairList ((k,v) : filter ((/=k) . fst) xs)

    lookup k (PairList xs) =
        case [v | (k',v) <- xs, k' == k] of
          []    -> Nothing
          (v:_) -> Just v

    delete k (PairList xs) =
        PairList (filter ((/=k) . fst) xs)

    toList (PairList xs) = xs



-- #######################################################
-- 3
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
      deriving Show

instance Collection SearchTree where
  empty = Empty

  singleton k v = BNode Empty k (Just v) Empty

  insert k v Empty = singleton k v
  insert k v (BNode left key val right)
    | k < key   = BNode (insert k v left) key val right
    | k > key   = BNode left key val (insert k v right)
    | otherwise = BNode left key (Just v) right
    --daca cheia exista ii schimb valoarea

  delete _ Empty = Empty
  delete k (BNode left key val right)
    | k < key   = BNode (delete k left) key val right
    | k > key   = BNode left key val (delete k right)
    | otherwise = BNode left key Nothing right
    -- marcare ca sters => valoarea devine Nothing

  toList Empty = []
  toList (BNode left key val right) =
    toList left ++ case val of
                     Nothing -> toList right
                     Just v  -> (key,v) : toList right


test31 =
  let t = fromList [(5,"a"),(3,"b"),(7,"c")] :: SearchTree Int String
  in keys t == [3,5,7] && values t == ["b","a","c"]

test32 =
  let t = insert 4 "x" (fromList [(5,"a"),(3,"b")]) :: SearchTree Int String
  in keys t == [3,4,5] && values t == ["b","x","a"]

test33 =
  let t = delete 5 (fromList [(5,"a"),(3,"b"),(7,"c")]) :: SearchTree Int String
  in keys t == [3,7] && values t == ["b","c"]



-- #######################################################
-- 4

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving (Show, Eq)

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a

instance Show Punct where 
    show (Pt []) = "()"
    show (Pt xs) = "(" ++ go xs ++")"
        where
            go [] = ""
            go [x] = show x
            go (x:xs) = show x ++ ", " ++ go xs

-- Pt [1,2,3]
-- (1, 2, 3)
test41 = show (Pt [1,2,3]) == "(1, 2, 3)"


-- Pt []
-- ()
test42 = show (Pt []) == "()"

-- #######################################################
-- 5

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)



instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N (F x) rest) =
        let Pt coords = fromArb rest
        in Pt (x : coords)

test51 = toArb (Pt [1,2,3]) == N (F 1) (N (F 2) (N (F 3) Vid))
test52 = toArb (Pt []) == Vid

-- #######################################################
-- 6

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance GeoOps Geo where
    perimeter (Square a) = 4 * a
    perimeter (Rectangle a b) = 2 * (a + b)
    perimeter (Circle r) = 2 * pi * r

    area (Square a) = a * a
    area (Rectangle a b) = a * b
    area (Circle r) = pi * r * r





-- #######################################################
-- 7

instance (Floating a, Eq a) => Eq (Geo a) where
  g1 == g2 = perimeter g1 == perimeter g2


test71 = Square 5 == Rectangle 10 0
test72 = Circle 2 == Square pi