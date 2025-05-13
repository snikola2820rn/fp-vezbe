import Control.Monad
import Text.Parsec
import Text.Read
import GHC.Internal.Text.Read.Lex (numberToFixed)


{-
    Napisati funkciju koja resava matematicku jednacinu u Reverse Polish Notation-u
    tako da radi i sa pogresnim unosom.
-}

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((x - y):ys)
foldingFunction xs numString = fmap (:xs) . readMaybe' $ numString
    where readMaybe' st = case reads st of [(x, "")] -> Just x
                                           _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do [res] <- foldM foldingFunction [] . words $ st
                 return res

{-
    Napisati funkciju koja resava matematicku jednacinu u Reverse Polish Notation-u
    tako da radi i sa pogresnim unosom i ispisuje poruku o gresci.
-}

foldingFunction' :: [Double] -> String -> Either String [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y):ys)
foldingFunction' (x:y:ys) "+" = return ((x + y):ys)
foldingFunction' (x:y:ys) "-" = return ((x - y):ys)
foldingFunction' xs numString = fmap (:xs) . readMaybe' $ numString
    where readMaybe' st = case reads st of [(x, "")] -> return x
                                           _ -> Left "Pogresan unos!"

solveRPN' :: String -> Either String Double
solveRPN' st = do res <- foldM foldingFunction' [] . words $ st
                  case length res of 1 -> return . head $ res
                                     _ -> Left "Nije kraj!"

{-
    Definisati tip binarno stablo i instancirati klasu Functor.
-}

data Stablo a = Prazno | Cvor a (Stablo a) (Stablo a) deriving Show

instance Functor Stablo where
    fmap _ Prazno = Prazno
    fmap f (Cvor x l r) = Cvor (f x) (fmap f l) (fmap f r)

{-
    Definisati tip kompleksnih brojeva i instancirati klase Show, Functor, Applicative i Monad.
-}

data Complex a = Complex a a

instance (Show a, Num a, Ord a) => Show (Complex a) where
    show (Complex re im) = show re ++ if im < 0 then " - " else " + " ++ show (abs im) ++ "i"

instance Functor Complex where
    fmap f (Complex re im) = Complex (f re) (f im)

instance Applicative Complex where
    pure x = Complex x x
    (Complex f1 f2) <*> (Complex x1 x2) = Complex (f1 x1) (f2 x1)

instance Monad Complex where
    return = pure
    (Complex re im) >>= f = let (Complex a b) = f re; (Complex c d) = f im in Complex a d

{-
    Napisati parser za sabiranje i oduzimanje kompleksnih brojeva.
-}

number :: Parsec String () String
number = many1 digit

plus :: Parsec String () String
plus = char '+' >> spaces >> number

minus :: Parsec String () String
minus = (:) <$> char '-' <*> (spaces >> number)

complexB :: Parsec String () (Complex Double)
complexB = do spaces
              r <- readMaybe <$> do num <- plus <|> minus <|> number
                                    spaces
                                    notFollowedBy (char 'i')
                                    return num
              i <- readMaybe <$> do num <- plus <|> minus <|> number
                                    spaces
                                    char 'i'
                                    return num
              case r of Just x -> case i of Just y -> return (Complex x y)
                                            Nothing -> return (Complex x 0)
                        Nothing -> case i of Just y -> return (Complex 0 y)
                                             Nothing -> return (Complex 0 0)

complexR :: Parsec String () (Complex Double)
complexR = do spaces
              r <- readMaybe <$> do num <- plus <|> minus <|> number
                                    spaces
                                    notFollowedBy (char 'i')
                                    return num
              case r of Just x -> return (Complex x 0)
                        Nothing -> return (Complex 0 0)

complexI :: Parsec String () (Complex Double)
complexI = do spaces
              i <- readMaybe <$> do num <- plus <|> minus <|> number
                                    spaces
                                    char 'i'
                                    return num
              case i of Just y -> return (Complex 0 y)
                        Nothing -> return (Complex 0 0)

complex :: Parsec String () (Complex Double)
complex = try complexB <|> try complexR <|> complexI

operation :: Num a => Parsec String () (a -> a -> a)
operation = do spaces
               op <- oneOf "+-"
               spaces
               return $ case op of '+' -> (+)
                                   '-' -> (-)

eq :: Parsec String () (Complex Double)
eq = do spaces
        x <- complex
        spaces
        op <- operation
        spaces
        y <- complex
        return $ op <$> x <*> y

calc :: Parsec String () (Complex Double)
calc = try eq <|> complex

{-
    Napisati program koji sa komandne linije ucitava jednacine sa kompleksnim brojevima
    i ispisuje rezultat. Program se zavrsava unosom prazne linije.
-}

main = do line <- getLine
          if null line then return ()
                       else do case parse calc "" line of Left err -> print err
                                                          Right res -> print res
                               main
