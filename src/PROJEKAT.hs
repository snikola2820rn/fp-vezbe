import Data.List

data Rose a = Node a [Rose a] deriving Show

size :: Rose a -> Int
size (Node _ ch) = 1 + (sum . map size $ ch)

length :: Rose a -> Int
length (Node _ []) = 1
length (Node _ ch) = 1 + (maximum . map Main.length $ ch)

leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ ch) = sum . map leavesCount $ ch

leaves :: Rose a -> [a]
leaves (Node x []) = [x]
leaves (Node x ch) = concatMap leaves ch

instance Functor Rose where
    fmap f (Node x ch) = Node (f x) (map (fmap f) ch)

elemsOnDepth :: Rose a -> Int -> [a]
elemsOnDepth _ 0 = error "Vece od 0!"
elemsOnDepth (Node x _) 1  = [x]
elemsOnDepth (Node _ []) _ = []
elemsOnDepth (Node _ ch) n  = concatMap (flip elemsOnDepth (n-1)) ch

foldRose ::  (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node x ch) = foldl (\acc1 node -> foldRose f acc1 node) (f x acc) ch

rt = Node 1 [Node 2 [], Node 3 [Node 4 [Node 8 []]], Node 5 [Node 6 [], Node 7 []]]

data Player = First | Second deriving (Show, Eq)

data Game a = Game {player :: Player, board :: Board a}

data Board a = Board {dim :: Int, cells :: [[a]]}

newtype GameStateOp b a = GameStateOp {runGame :: Game b -> (a, Game b)}

instance Functor (GameStateOp b) where
    fmap f (GameStateOp rs) = GameStateOp $ \s -> let (a, newState) = rs s in (f a, newState)

instance Applicative (GameStateOp b) where
    pure x = GameStateOp $ \s -> (x, s)
    (GameStateOp rs1) <*> (GameStateOp rs2) = GameStateOp $ \s -> let 
                                                                (f, newState) = rs1 s
                                                                (a, finalState) = rs2 newState
                                                              in (f a, finalState)

instance Monad (GameStateOp b) where
    return = pure
    (GameStateOp rs) >>= f = GameStateOp $ \s -> let 
                                                    (a, newState) = rs s
                                                    (GameStateOp h) = f a
                                                in h newState

newtype GameStateOpHistory b a = GameStateOpHistory {runGameHistory :: Game b -> (a, [Game b])}

instance Functor (GameStateOpHistory b) where
    fmap f (GameStateOpHistory rs) = GameStateOpHistory $ \s -> let (a, history) = rs s in (f a, history)

instance Applicative (GameStateOpHistory b) where
    pure x = GameStateOpHistory $ \s -> (x, [s])
    (GameStateOpHistory rs1) <*> (GameStateOpHistory rs2) = GameStateOpHistory $ \s -> let 
                                                                                    (f, history1@(ls:_)) = rs1 s
                                                                                    (a, history2) = rs2 ls
                                                                                    in (f a, history2 ++ history1)

instance Monad (GameStateOpHistory b) where
    return = pure
    (GameStateOpHistory rs) >>= f = GameStateOpHistory $ \s -> let 
                                                                (a, history@(ls:_)) = rs s
                                                                (GameStateOpHistory h) = f a
                                                                (b, newHistory) = h ls
                                                            in (b, newHistory ++ history)

instance Show (Board Int) where
    show (Board _ rows) = unlines $ map showRow rows
                        where 
                            showRow row = "|" ++ concatMap (\cell -> (case cell of
                                0 -> " "
                                1 -> "X"
                                2 -> "O") ++ "|") row 

instance Show (Game Int) where
    show (Game p b) = "Player: " ++ show p ++ "\n" ++ show b
    
iksOks :: Game Int
iksOks = Game First (Board 3 [[0,0,0],[0,0,0],[0,0,0]])

validMoves :: Board Int -> [(Int, Int)]
validMoves (Board dim rows) = [(i, j) | i <- [0..dim-1], j <- [0..dim-1], (rows !! i) !! j == 0]

makeMove :: Game Int -> (Int, Int) -> Game Int
makeMove (Game p (Board dim rows)) (i, j) =
    Game (if p == First then Second else First) 
        (Board dim (take i rows ++ [take j (rows !! i) ++ [if p == First then 1 else 2] ++ drop (j + 1) (rows !! i)] ++ drop (i + 1) rows))

isFinished :: Game Int -> Bool
isFinished (Game _ (Board dim rows)) = all (/= 0) (concat rows) 
                                    || any (\row -> all (== 1) row || all (== 2) row) rows 
                                    || any (\col -> all (== 1) col || all (== 2) col) (transpose rows)
                                    || all (== 1) [rows !! i !! i | i <- [0..dim - 1]]
                                    || all (== 2) [rows !! i !! i | i <- [0..dim - 1]]
                                    || all (== 1) [rows !! i !! (dim - 1 - i) | i <- [0..dim - 1]]
                                    || all (== 2) [rows !! i !! (dim - 1 - i) | i <- [0..dim - 1]]

gameTree :: Game Int -> Rose (Game Int)
gameTree game@(Game _ (Board dim rows)) = Node game [gameTree (makeMove game move) | move <- validMoves (board game)]

applyMove :: (Int, Int) -> GameStateOp Int Bool
applyMove pos = GameStateOp $ \s -> let
                                        ns = makeMove s pos
                                        fin = Main.isFinished ns
                                    in (fin, ns)

applyMoves :: GameStateOp Int Bool
applyMoves = do
    applyMove (0,1)
    applyMove (1,0)
    applyMove (0,0)
    applyMove (0,2)
    applyMove (1,1)
    applyMove (2,0)
    applyMove (2,2)

applyMoveH :: (Int, Int) -> GameStateOpHistory Int Bool
applyMoveH pos = GameStateOpHistory $ \s -> let
                                        ns = makeMove s pos
                                        fin = Main.isFinished ns
                                    in (fin, [ns])

initialize :: GameStateOpHistory Int Bool
initialize = GameStateOpHistory $ \s -> (False, [s])

applyMovesH :: GameStateOpHistory Int Bool
applyMovesH = do
    initialize
    applyMoveH (0,1)
    applyMoveH (1,0)
    applyMoveH (0,0)
    applyMoveH (0,2)
    applyMoveH (1,1)
    applyMoveH (2,0)
