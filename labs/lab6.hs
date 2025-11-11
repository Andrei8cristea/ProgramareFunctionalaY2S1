-- #######################################################
-- Apple and Oranges
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

-- #######################################################
-- A

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) | soi `elem` ["Tarocco", "Moro", "Sanguinello"] = True
ePortocalaDeSicilia _ = False
test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

-- #######################################################
-- B

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (h:t) 
    | ePortocalaDeSicilia h= felii h + nrFeliiSicilia t  
    | otherwise = nrFeliiSicilia t
    where
        felii (Portocala _ n) = n
        felii _ = 0

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

-- #######################################################
-- c
nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (h:t)
    | areViermi h = 1 + nrMereViermi t
    | otherwise = nrMereViermi t
    where 
        areViermi (Mar _ bool) = bool
        areViermi _ = False

test_nrMereViermi = nrMereViermi listaFructe == 2

-- #######################################################
-- Paw Patrol

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

-- #######################################################
-- a

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meaw!"
vorbeste (Caine _ _) = "Woof!"

pis = Pisica "Tom"
catel = Caine "Spike" "Bulldog"

--vorbeste pis
--vorbeste catel

-- #######################################################
-- b
rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Pisica _) = Nothing

-- #######################################################
-- Matrix Resurrections
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

-- #######################################################
-- a
verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L xs) rest -> sum xs == n && rest) True linii
test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

-- #######################################################
-- b
doarPozN :: Matrice -> Int -> Bool
doarPozN (M []) _= True
doarPozN (M(L h:t)) n
    | length h == n = all(>0) h && doarPozN (M t) n   
    |otherwise = doarPozN (M t) n


testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

-- #######################################################
-- c
corect :: Matrice -> Bool
corect (M []) = True
corect (M (L h:t)) = all (\(L h2) -> length h2 == length h) t

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True

