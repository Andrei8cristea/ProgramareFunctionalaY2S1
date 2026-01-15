-- #######################################################
-- 1


{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

-- #######################################################
-- 1


pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct_do :: Maybe Int -> Maybe Bool
fct_do mx = do
    x <- mx -- extrag valoarea din maybe int si o pun in x
    return (pos x)


-- #######################################################
-- 2 A

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just x) (Just y) = Just (x + y) -- suma celor doua maybe int uri
addM _ _ = Nothing --daca una e nothing returnez nothing


-- #######################################################
-- 2 B

addM_do :: Maybe Int -> Maybe Int -> Maybe Int
addM_do mx my = do
    x <- mx
    y <- my -- extrag valorile din cele doua maybe int uri
    return (x + y) -- returnez suma lor in maybe int


-- #######################################################
-- 3


cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product_do :: [a] -> [b] -> [(a,b)] 
cartesian_product_do xs ys = do 
    x <- xs 
    y <- ys 
    return (x, y)


prod f xs ys = [f x y | x <- xs, y<-ys]

prod_do f xs ys = do
    x <- xs
    y <- ys
    return (f x y) -- aplic f pe toate perechile

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine_do :: IO String
myGetLine_do = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- myGetLine_do --returnez caractere pana gasesc newline
        return (x : xs)

-- #######################################################
-- 4
prelNo :: Float -> Float
prelNo x = sqrt x


ioNumber :: IO ()
ioNumber =
    (readLn :: IO Float) >>= \noin ->
    putStrLn ("Intrare\n" ++ show noin) >>
    let noout = prelNo noin in
    putStrLn "Iesire" >>
    print noout

ioNumber_do = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout



-- #######################################################
-- 5





--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell ("increment:" ++ show x ++ "\n")
    return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

-- ###############################################
-- B
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 0 = return x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n - 1)

newtype WriterLS a = WriterLS { runWriterLS :: (a, [String]) }

instance Monad WriterLS where
    return a = WriterLS (a, [])
    ma >>= k =
        let (a, log1) = runWriterLS ma
            (b, log2) = runWriterLS (k a)
        in WriterLS (b, log1 ++ log2)

instance Applicative WriterLS where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor WriterLS where
    fmap f ma = pure f <*> ma

tellLS :: String -> WriterLS () 
tellLS msg = WriterLS ((), [msg])



logIncrement_LS :: Int -> WriterLS Int
logIncrement_LS x = do
    tellLS ("increment:" ++ show x)
    return (x + 1)


logIncrementN_LS :: Int -> Int -> WriterLS Int
logIncrementN_LS x 0 = return x
logIncrementN_LS x n = do
    y <- logIncrement_LS x
    logIncrementN_LS y (n - 1)

-- C



   
--------------------------------------------------------




-- ######################################################
-- 6
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN = undefined
showPersonA :: Person -> String
showPersonA = undefined

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson = undefined 

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

mshowPersonN ::  Reader Person String
mshowPersonN = undefined
mshowPersonA ::  Reader Person String
mshowPersonA = undefined 
mshowPerson ::  Reader Person String
mshowPerson = undefined 
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}