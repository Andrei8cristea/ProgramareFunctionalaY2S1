-- #######################################################
--un wrapper in jurul unei singure valori fmap aplica doar functia asupra acelei valori
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity x) = Identity (f x)

--#######################################################
--detine doua valori de acelasi tip fmap aplica functia la ambele
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = Pair (f x) (f y)

--#######################################################
--stocheaza o valoare de tip b dar ignora a fmap aplica functia asupra b
data Constant a b = Constant b deriving (Eq, Show)
instance Functor (Constant a) where
  fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap f (Constant y) = Constant (f y)

--#######################################################
--detine o valoare fixa a si o valoare mapata b fmap modifica doar al doilea
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap :: (b -> c) -> Two a b -> Two a c
  fmap f (Two x y) = Two x (f y)

--#######################################################
--detine trei valori dar doar ultima este mapata
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap :: (c -> d) -> Three a b c -> Three a b d
  fmap f (Three x y z) = Three x y (f z)

--#######################################################
--detine un a si doi bs fmap aplica functia la ambii bs
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap :: (b -> c) -> Three' a b -> Three' a c
  fmap f (Three' x y z) = Three' x (f y) (f z)

--#######################################################
--detine patru valori doar ultima este mapata
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap :: (d -> e) -> Four a b c d -> Four a b c e
  fmap f (Four w x y z) = Four w x y (f z)

--#######################################################
--detine trei as fixe si un b fmap aplica functia asupra b
data Four'' a b = Four'' a a a b deriving (Eq, Show)
instance Functor (Four'' a) where
  fmap :: (b -> c) -> Four'' a b -> Four'' a c
  fmap f (Four'' w x y z) = Four'' w x y (f z)

--#######################################################
--un tip suma cu trei constructori
--finance fara mapare
--desk a ignora maparea
--bloor b aplica functia asupra b
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap :: (b -> c) -> Quant a b -> Quant a c
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

--#######################################################
--wrapeaza un alt functor f fmap delega catre functorul interior
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance Functor f => Functor (LiftItOut f) where
  fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--#######################################################
--detine doi functori f si g fmap aplica functia la ambii
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

--#######################################################
--detine f a si g b doar g b este mapatat
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap :: (b -> c) -> IgnoreOne f g a b -> IgnoreOne f g a c
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

--#######################################################
--detine trei valori wrappate de functor doar ultima este mapata
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)
instance Functor g => Functor (Notorious g o a) where
  fmap :: (t -> u) -> Notorious g o a t -> Notorious g o a u
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

--#######################################################
--un tip recursiv
--nogoat nimic de mapatat
--onegoat a aplica functia asupra a
--moregoats aplica fmap recursiv fiecarei ramuri
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)
instance Functor GoatLord where
  fmap :: (a -> b) -> GoatLord a -> GoatLord b
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

--#######################################################
--reprezinta o structura interactiva
--halt nimic de mapatat
--print string a aplica functia asupra a
--read (string -> a) compune functia cu reader ul
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)

--#######################################################
--teste

testIdentity1 :: Bool
testIdentity1 = fmap (+1) (Identity (1 :: Int)) == Identity (2 :: Int)

testIdentity2 :: Bool
testIdentity2 = fmap (++ " world") (Identity "hello") == Identity "hello world"

testPair1 :: Bool
testPair1 = fmap (*2) (Pair (3 :: Int) 7) == Pair 6 14

testConstant :: Bool
testConstant = fmap (++ " world") (Constant "hello" :: Constant Int String) == Constant "hello world"

testTwo1 :: Bool
testTwo1 = fmap (+10) (Two "fixed" (5 :: Int)) == Two "fixed" 15

testThree1 :: Bool
testThree1 = fmap (^2) (Three 'a' True (4 :: Int)) == Three 'a' True 16

testThree' :: Bool
testThree' = fmap (++ "!") (Three' "x" "a" "b") == Three' "x" "a!" "b!"

testFour :: Bool
testFour = fmap (+1) (Four 1 2 3 (4 :: Int)) == Four 1 2 3 5

testFour'' :: Bool
testFour'' = fmap (++ "?") (Four'' "a" "b" "c" "z") == Four'' "a" "b" "c" "z?"

testQuant1 :: Bool
testQuant1 = fmap (+1) (Bloor (10 :: Int) :: Quant String Int) == Bloor 11

testQuant2 :: Bool
testQuant2 = fmap (+1) (Desk "office" :: Quant String Int) == Desk "office"

testLiftItOut :: Bool
testLiftItOut = fmap (+1) (LiftItOut (Just (1 :: Int))) == LiftItOut (Just 2)

testParappa :: Bool
testParappa = fmap (+1) (DaWrappa (Just (1 :: Int)) [2]) == DaWrappa (Just 2) [3]

testIgnoreOne :: Bool
testIgnoreOne = fmap (+1) (IgnoringSomething (Left "x" :: Either String Int) (Just (2 :: Int))) == IgnoringSomething (Left "x") (Just 3)

testNotorious :: Bool
testNotorious = fmap (+1) (Notorious (Just 'a') (Just True) (Just (3 :: Int))) == Notorious (Just 'a') (Just True) (Just 4)

testGoatLord :: Bool
testGoatLord = fmap (+1) (MoreGoats (OneGoat (1 :: Int)) NoGoat (OneGoat 3)) == MoreGoats (OneGoat 2) NoGoat (OneGoat 4)

testAll :: Bool
testAll = and
  [ testIdentity1 testIdentity2
  , testPair1
  , testConstant
  , testTwo1
  , testThree1
  , testThree'
  , testFour
  , testFour''
  , testQuant1 testQuant2
  , testLiftItOut
  , testParappa
  , testIgnoreOne
  , testNotorious
  , testGoatLord
  ]