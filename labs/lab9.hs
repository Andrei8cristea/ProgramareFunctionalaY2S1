-- #######################################################
-- 1

data Tree = Empty  -- arbore vid
  | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina
                            -- si 3 fii
  
extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) 
                (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

--    4
--  / | \
-- 5  3 ( )
--     \
--      1



class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; 
                    -- consideram ca un arbore vid are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui


instance ArbInfo Tree where
  level Empty = 0
  level (Node _ t1 t2 t3) = 1 + maximum [level t1, level t2, level t3]

  sumval Empty = 0
  sumval (Node v t1 t2 t3) = v + sumval t1 + sumval t2 + sumval t3

  nrFrunze Empty = 0
  nrFrunze (Node _ Empty Empty Empty) = 1
  nrFrunze (Node _ t1 t2 t3) = nrFrunze t1 + nrFrunze t2 + nrFrunze t3


test11 = level extree == 3
test12 = sumval extree == 13
test13 = nrFrunze extree == 2





-- #######################################################
-- 2

class Scalar a where
  zero :: a 
  one :: a 
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

instance Scalar Double where
  -- folosesc Double deoarce daca aveam int si faceam inversul de cele mai
  -- multe ori as fi obtinut un numar rational
  zero = 0.0
  one = 1.0
  adds x y = x + y
  mult x y = x * y
  negates x = -x
  recips x = 1.0 / x

instance Scalar Float where
  
  zero = 0.0
  one = 1.0
  adds x y = x + y
  mult x y = x * y
  negates x = -x
  recips x = 1.0 / x


test21 :: Bool
test21 = adds (1.5 :: Double) (2.5 :: Double) == 4.0

test22 :: Bool
test22 = mult (2.5 :: Double) (4.0 :: Double) == 10.0

test23 :: Bool
test23 = negates (-2.75 :: Double) == 2.75

test24 :: Bool
test24 = recips (2.0 :: Double) == 0.5
alltests2 = and [test21, test22, test23, test24]






-- #######################################################
-- 3

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector


data Vec2 a = Vec2 a a
  deriving (Eq, Show)

data Vec3 a = Vec3 a a a
  deriving (Eq, Show)


--INSTANTA PENTRU VEC BIDIMENSIONAL
instance Scalar a => Vector Vec2 a where
  zerov = Vec2 zero zero
  onev = Vec2 one one
  addv (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (adds x1 x2) (adds y1 y2)
  smult s (Vec2 x y) = Vec2 (mult s x) (mult s y)
  negatev (Vec2 x y) = Vec2 (negates x) (negates y)


test31 = (addv (Vec2 (1.5 :: Double) (2.0 :: Double))
               (Vec2 (0.5 :: Double) (3.0 :: Double))
         :: Vec2 Double) == Vec2 2.0 5.0


test32 = (smult (2.0 :: Double) (Vec2 (1.5 :: Double) (2.0 :: Double))
         :: Vec2 Double) == Vec2 3.0 4.0


test33 = negatev (Vec2 (1.0 :: Double) (-2.5 :: Double)) == Vec2 (-1.0) 2.5

alltest3 = and [test31, test32, test33]


--INSTANTA PENTRU VEC BIDIMENSIONAL

instance Scalar a => Vector Vec3 a where
  zerov = Vec3 zero zero zero
  onev = Vec3 one one one
  addv (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = 
    Vec3 (adds x1 x2) (adds y1 y2) (adds z1 z2)

  smult s (Vec3 x y z) = Vec3 (mult s x) (mult s y) (mult s z)

  negatev (Vec3 x y z) = Vec3 (negates x) (negates y) (negates z)


test34 = (addv (Vec3 (1.0 :: Double) (2.5 :: Double) (3.0 :: Double))
               (Vec3 (4.0 :: Double) (5.5 :: Double) (6.0 :: Double))
         :: Vec3 Double) == Vec3 5.0 8.0 9.0

test35 = (smult (2.5 :: Double) (Vec3 (1.0 :: Double) (2.0 :: Double) (3.0 :: Double))
         :: Vec3 Double) == Vec3 2.5 5.0 7.5

test36 = negatev (Vec3 (1.0 :: Double) (-2.0 :: Double) (3.0 :: Double))
         == Vec3 (-1.0) 2.0 (-3.0)

alltest4 = and [test34, test35, test36]




