import Data.Foldable (Foldable(toList))
{-
    Grupisati uzastopne elemente liste u podliste koje sadrze samo te elemente.
    ['a','a','a','b','c','c'] -> [['a','a','a'], ['b'], ['c','c']]
-}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : (pack . dropWhile (== x) $ xs)

{-
    Grupisati uzastopne elemente liste u tuple-ove (<element>, <broj_ponavljanja> :: Int).
-}

pack' :: Eq a => [a] -> [(a, Int)]
pack' [] = []
pack' xxs = [ (head xs, length xs) | xs <- pack xxs]

{-
    Definisati tip koji predstavlja tupleove iz prethodnog zadatka tako da razlikuje ako se elemet
    ponavlja vise puta ili samo jednom. Zatim prepraviti funkciju iz prethodnog zadatka da vraca listu
    elemenata navedenog tipa.
-}

data MyPacked a = Single a | Multiple a Int

instance Eq a => Eq (MyPacked a) where
    (Single x) == (Single y) = x == y
    (Multiple x n) == (Multiple y m) = x == y && n == m
    _ == _ = False

pack'' :: Eq a => [a] -> [MyPacked a]
pack'' [] = []
pack'' xs =[ if i == 1 then Single x else Multiple x i | (x, i) <- pack' xs]

{-
    Napisati funkciju koja pretvara rezultat prethodnog zadatka nazad u listu.
-}

unpack :: [MyPacked a] -> [a]
unpack [] = []
unpack (x:xs) = decode x ++ unpack xs
    where decode (Single x) = [x]
          decode (Multiple x n) = replicate n x

{-
    Napisati instancu klase show za tip iz prethodnog zadatka da prikazuje podatke u citljivom obliku.
-}

instance Show a => Show (MyPacked a) where
    show (Single x) = show x
    show (Multiple x n) = show x ++ "x" ++ show n


{-
    Definisati tip i funkcije (insert i elem) za binary search tree. 
-}

data Stablo a = Prazno | Cvor a (Stablo a) (Stablo a) deriving Show

insert :: Ord a => a -> Stablo a -> Stablo a
insert x Prazno = Cvor x Prazno Prazno
insert x (Cvor y levo desno)
    | x < y = Cvor y (insert x levo) desno
    | x > y = Cvor y levo (insert x desno)
    | otherwise = Cvor y levo desno

nadji :: Ord a => a -> Stablo a -> Bool
nadji _ Prazno = False
nadji x (Cvor y levo desno)
    | x < y = nadji x levo
    | x > y = nadji x desno
    | otherwise = True

{-
    Napraviti funkcije koje pretvaraju listu u stablo i stablo u listu.
-}

fromList :: Ord a => [a] -> Stablo a
fromList xs = foldr insert Prazno xs

toList' :: Stablo a -> [a]
toList' Prazno = []
toList' (Cvor x levo desno) = (toList' levo) ++ [x] ++ (toList' desno)


{-
    Napisati funkciju koja rotira triple za zadati broj mesta u levo.
    Broj mesta se zadaje tipom koji moze imati 3 vrednosti.
-}

data Broj = Nula | Jedan | Dva

rotate :: Broj -> (a, a, a) -> (a, a, a)
rotate Nula (x, y, z) = (x, y, z)
rotate Jedan (x, y, z) = (y, z, x)
rotate Dva (x, y, z) = (z, x, y)

{-
    Napisati funkciju koja reci u recenici ispisuje u nazad, a ostavlja ih na istom mestu u recenici.
-}

uNazad :: String -> String
uNazad "" = ""
uNazad str = unwords . map reverse . words $ str

{-
    Napisati program koji sa standardnog ulaza ucitava recenice i ispisuje ih u nazad kao u prethodnom zadatku.
    Osim u slucaju da je recenica palindrom. Tada ispisuje "Palindrom".
-}

palindrom :: String -> Bool
palindrom str = str == reverse str

rmSpaces :: String -> String
rmSpaces str = foldl (\s acc -> s ++ acc) "" . words $ str

printLine :: String -> IO ()
printLine "" = putStrLn ""
printLine str
    | palindrom . rmSpaces $ str = putStrLn "Palindrom"
    | otherwise = putStrLn . uNazad $ str

main :: IO ()
main = do line <- getLine
          if null line then return ()
                       else do printLine line
                               main

{-
    Napisati tip Imenik koji sadrzi ime, broj telefona i e-mail.
-}

data Imenik = Imenik [Osoba]
data Osoba = Osoba { ime :: String, telefon :: String, mail :: String }

{-
    Napisati funkciju koja za zadati Imenik vraca list tuple-ova koji sadrze samo ime i broj telefona.
-}

noMail :: Imenik -> [(String, String)]
noMail (Imenik []) = []
noMail (Imenik (x:xs)) = (ime x, telefon x) : noMail (Imenik xs)
