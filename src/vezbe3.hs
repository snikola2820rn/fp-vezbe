import Distribution.Compat.Lens (_1)
{-  Jos malo funkcija

    Guard
        - slicno kao pattern ali ne proverava jednakost nego zadajemo Bool izraz
        - otherwise - ako svi prethodni vrate False (slicno kao "_")
        - rade u kombinaciji sa patternima (proveravaju se redom kojim su navedeni)
    
    Where
        - binduje novo ime unutar guardova
            - npr. ako se neko izracunavanje ponavlja u vise guardova
        - povecava citljivost i performanse
        - imena se ne dele izmedju tela funkcije (pattern)
        - imena mogu biti konstante ali mogu i funkcije
        - mogu biti ugnjezdene
        - mozemo ih pisati inline razdvojene sa ";"
    
    Let
        - let <bindings> in <expression>
        - slicno kao where ali su imena vidljiva samo u in izrazu
        - ponasaju se kao izraz pa mogu da se koriste bilo gde
        - mozemo ih pisati inline razdvojene sa ";"
        - ako ih koristimo kao predikat u list comprehensionu imena su vidljiva izlaznoj funkciji
            - ako stavimo "in" onda su vidljiva samo tom predikatu
    
    Case
        - case expression of
                   pattern -> result
                   pattern -> result
                   pattern
                      | guadr -> result
                      | guard -> result
                   ...
                   otherwise -> result
        - takodje izraz i moze se koristiti bilo gde
        - slicno kao pattern matching i guard
    
    Rekurzija
        - "Recursion is important to Haskell because unlike imperative languages, you do computations
           in Haskell by declaring what something is instead of declaring how you get it."
        - granicni slucajevi odredjuju kada se rekurzija prekida
            - validno je definisati beskonacnu rekurzivnu funkciju

-}

test :: [Int] -> String
test [x] | x == 1 = "jedan" | x > 1 = "vece" | otherwise = "manje"
test c@(x:xs)
    | length c == 1 = "jedan"
    | length c > 1 = "vece"
    | otherwise = "manje"

comp :: Ord a => a -> a -> Ordering
x `comp` y
    | x > y = GT
    | x == y = EQ
    | otherwise = LT

-- comp' :: Int -> Int -> String
-- comp' x y
--     | zbir 1 2 > broj = "vece"
--     | zbir == broj = "jednako"
--     | zbir < broj = "manje"
--     where broj = 5
--           zbir a b = a + b + x + y
-- comp' (x:xs) y
--     | zbir 1 2 > broj = "vece"
--     | zbir == broj = "jednako"
--     | zbir < broj = "manje"

init' :: String -> String -> String
init' fname lname = ime ++ ": " ++ [f] ++ ". " ++ [l] ++ "."
        where ime = fname ++ " " ++ lname
              (f:_) = fname
              (l:_) = lname

cyl :: Float -> Float -> Float
cyl r h = 2 + let side = 2
                  top = 2
              in side + 2 * top

primer :: (Num a, Ord a) => [(a, a)] -> [a]
primer xs = [fst x + snd x | x <- xs, let (a, b) = x in a + b > 3]

head' :: [a] -> String
head' xs = "Lista je: " ++ case xs of [] -> "prazna"
                                      [x] -> "singleton"
                                      (x:_) -> "duza"

max' :: Ord a => [a] -> a
max' [] = error "prazna"
max' [x] = x
max' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = max' xs

repl :: (Num i, Ord i) => i -> a -> [a]
repl n x
    | n <= 0 = []
    |otherwise = x : repl (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rep :: a -> [a]
rep x = x : rep x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = let smSorted = qsort [a | a <- xs, a <= x]
                   lgSorted = qsort [a | a <- xs, a > x]
               in smSorted ++ [x] ++ lgSorted


lastElem :: [a] -> a
lastElem [x,_] = x
lastElem (x:xs) = lastElem xs

kthElem :: Int -> [a] -> a 
kthElem n xs
        | n < 0 = error "negativan"
        | n >= (length xs) = error "mnogo"
kthElem _ [] = error "prazna"
kthElem 0 (x:xs) = x
kthElem n (x:xs) = kthElem (n - 1) xs

numElems :: [a] -> Int
numElems [] = 0
numElems (x:xs) = 1 + numElems xs

revList :: [a] -> [a]
revList [] = []
revList (x:xs) = revList xs ++ [x]

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == (revList xs)

-- flatten :: [a] -> [a]
-- flatten [x] = [x]
-- flatten (x:xs) = flatten x ++ flatten xs

foldl' :: Foldable t => (b->a->b)->b->t a->b
foldl' _ x  = 