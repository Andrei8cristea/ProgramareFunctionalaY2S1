import Control.Arrow (ArrowApply(app))
import Language.Haskell.TH (safe)
{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"
--Just 5

Just (++" world") <*> Just "hello,"
--Just "hello, world"

pure (+) <*> Just 3 <*> Just 5
--Just 8

pure (+) <*> Just 3 <*> Nothing
--Nothing

(++) <$> ["ha","heh"] <*> ["?","!"]
--["ha?","ha!","heh?","heh!"]

--$ infix fmap
-}

-- #######################################################
-- 1

data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))


-- #######################################################
-- 2 A

data Dog = Dog {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n
    | n < 0     = Nothing
    | otherwise = Just n

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

-- #######################################################
-- 2 B

dogFromString :: String -> Int -> Int -> Maybe Dog
dogFromString n a w = 
    case noEmpty n of
        Nothing -> Nothing
        Just validName ->
            case noNegative a of
                Nothing -> Nothing
                Just validAge ->
                    case noNegative w of
                        Nothing -> Nothing
                        Just validWeight ->
                            Just (Dog {name = validName, age = validAge, weight = validWeight})

test24 = dogFromString "Toto" 5 11 == Just (Dog {name = "Toto", age = 5, weight = 11})

-- #######################################################
-- 2 C

dogFromStringA :: String -> Int -> Int -> Maybe Dog
dogFromStringA n a w = 
    Dog <$> noEmpty n <*> noNegative a <*> noNegative w

-- functiile alea imi returneaza maybe string maybe string maybe int 
-- si rezultatul imi da dog doar daca toate erau just string just int just int

test25 = dogFromStringA "Toto" 5 11 == Just (Dog {name = "Toto", age = 5, weight = 11})
test26 = dogFromStringA "" 5 11 == Nothing

-- #######################################################
-- 3 A

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength n s
    | length s >= n = Nothing
    | otherwise = Just s

test31 = validateLength 5 "abc" == Just "abc"

-- #######################################################
-- 3 B

mkName :: String -> Maybe Name
mkName s = 
    case validateLength 25 s of
        Nothing -> Nothing
        Just validString -> Just (Name validString)

mkAddress :: String -> Maybe Address
mkAddress s = 
    case validateLength 100 s of
        Nothing -> Nothing
        Just validString -> Just (Address validString)

test32 = mkName "Popescu" ==  Just (Name "Popescu")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")


-- #######################################################
-- 3 C

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just validName ->
            case mkAddress a of
                Nothing -> Nothing
                Just validAddress -> Just (Person validName validAddress)

test34 = mkPerson "Popescu" "Str Academiei" == Just (Person (Name "Popescu") (Address "Str Academiei"))

-- #######################################################
-- 3 D

mknameA :: String -> Maybe Name
mknameA s = Name <$> validateLength 25 s

mkAddressA :: String -> Maybe Address
mkAddressA s = Address <$> validateLength 100 s

mkPersonA :: String -> String -> Maybe Person
mkPersonA n a = Person <$> mknameA n <*> mkAddressA a

test35 = mkPersonA "Popescu" "Str Academiei" == Just (Person (Name "Popescu") (Address "Str Academiei"))



