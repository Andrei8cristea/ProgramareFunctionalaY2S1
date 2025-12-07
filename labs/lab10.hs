import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:
infixr 1 :->:
infixr 1 :<->:

-- #######################################################
-- 1


pa :: Prop
pa = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

pb :: Prop
pb = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

pc :: Prop
pc = (Var "P" :&: (Var "Q" :|: Var "R"))
     :&: ((Not (Var "P") :|: Not (Var "Q"))
     :&: (Not (Var "P") :|: Not (Var "R")))


-- #######################################################
-- 2

instance Show Prop where
    show (Var n) = n
    show F = "F"
    show T = "T"
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"




test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

-- #######################################################
-- 3

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var n) env = impureLookup n env
eval F _ = False
eval T _ = True
eval (Not p) env = not (eval p env)
eval (p :|: q) env = eval p env || eval q env
eval (p :&: q) env = eval p env && eval q env
eval (p :->: q) env = not (eval p env) || eval q env
eval (p :<->: q) env = eval p env == eval q env

 
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

-- #######################################################
-- 4

variabile :: Prop -> [Nume]
variabile (Var n) = [n]
variabile F = []
variabile T = []
variabile (Not p) = variabile p
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile (p :&: q) = nub (variabile p ++ variabile q)
variabile (p :->: q) = nub (variabile p ++ variabile q)
variabile (p :<->: q) = nub (variabile p ++ variabile q)
-- nub imi elimina duplicatele dintr o lista
 
test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


-- #######################################################
-- 5


envs :: [Nume] -> [Env]
envs xs = sequence [ [(x, False) , (x, True)] | x <- xs ]
 
--sequence ia lista de liste si genereaza produsul cartezian

test_envs :: Bool
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

-- #######################################################
-- 6

satisfiabila :: Prop -> Bool
satisfiabila p = any (\env -> eval p env) (envs (variabile p))
-- practic iau variabilele din propozitia p
-- le atribui toate combinatiile posibile de valori de adevar
-- le evaluez pe toate
-- folosesc any si daca cel putin una este adevarata atunci p este satisfiabila
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

-- #######################################################
-- 7

valida :: Prop -> Bool
valida p = satisfiabila (Not p) == False

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

-- #######################################################
-- 8

-- p -> q este echivalent cu ~p | q
-- p <-> q este echivalent cu (p -> q) & (q -> p)

-- #######################################################
-- 9

echivalenta :: Prop -> Prop -> Bool
echivalenta p q = valida (p :<->: q)

--daca p si q sunt echivalente atunci p <-> q este tautologie (valida)

 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))


