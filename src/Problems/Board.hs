module Problems.Board
    (
        Board,
        newBoard,
        isGoal,
        boardHeuristic,
        validMoves,
        printValidMoves,
        printBoards
    ) where


import Data.List
import Data.Maybe

data Row = Row [Piece] deriving (Eq)

data Board = Board [Row] deriving (Eq)

data Piece = Player | Goal | EmptySpace | Wall deriving(Eq, Enum)

instance Show Piece where
    show Player = "p"
    show Goal = "g"
    show EmptySpace = "."
    show Wall = "w"

instance Show Row where
    show (Row row) = foldr (\piece b -> (show piece)++"\t"++b) "\n" row

instance Show Board where
    show (Board rows) = foldr (\row b -> (show row)++b) "" rows

newBoard :: [[Char]] -> Board
newBoard rows = Board $ map (\row -> Row $ map newPiece row) rows

newPiece :: Char -> Piece
newPiece 'p' = Player
newPiece 'g' = Goal
newPiece '.' = EmptySpace
newPiece 'w' = Wall
newPiece p = error "not a valid piece in board"

moveCost = 1

validMoves :: Board -> [(Board, Int)]
validMoves board = 
    map fromJust $ filter isJust possibleMoves
    where
        possibleMoves = map (\(r,c,p) -> if isValidMove (r,c,p) board then movePlayer board (r,c) >>= (\b -> Just (b,moveCost)) else Nothing) (flattenBoard board)

movePlayer :: Board -> (Int, Int) -> Maybe Board
movePlayer board (row, col) =
    playerPosition board >>= (\(prow, pcol, _) -> movePiece (prow, pcol, EmptySpace) board) >>= (\b -> movePiece (row, col, Player) (unflattenBoard b)) >>=
        (\ps -> Just (unflattenBoard ps))

movePiece :: (Int, Int, Piece) -> Board -> Maybe [(Int, Int, Piece)]
movePiece (row, col, piece) board = findIndex (\(r,c,_) -> r == row && c == col) (flattenBoard board) >>=
    (\index -> Just (splitAt (index) (flattenBoard board)) ) >>=
    (\(xs,_:ys) -> Just ( xs++[(row,col,piece)]++ys) )


isValidMove :: (Int, Int, Piece) -> Board -> Bool
isValidMove (_, _, Player) _ = False
isValidMove (_, _, Wall) _ = False
isValidMove (row, col, _) board = rowDistance <= 1 && colDistance <= 1 && rowDistance + colDistance == 1
    where
        (prow, pcol, _) = fromJust $ playerPosition board
        rowDistance = (abs $ row - prow)
        colDistance = (abs $ col - pcol)

boardHeuristic :: Board -> Int
boardHeuristic board = case goalPosition board of
    Nothing -> 0
    Just (grow, gcol, _) -> (abs $ prow - grow) + (abs $ pcol - gcol)
        where
            (prow, pcol, _) = fromJust $ playerPosition board

playerPosition :: Board -> Maybe (Int, Int, Piece)
playerPosition board = piecePosition board Player

goalPosition :: Board -> Maybe (Int, Int, Piece)
goalPosition board = piecePosition board Goal

piecePosition :: Board -> Piece -> Maybe (Int, Int, Piece)
piecePosition board piece = let ps = flattenBoard board in do
    ix <- findIndex (\(_,_,p) -> p == piece) ps
    Just (ps!!ix)

flattenBoard :: Board -> [(Int, Int, Piece)]
flattenBoard (Board rows) = foldr (\indexedRow b -> (flattenRow (fst indexedRow) (snd indexedRow)) ++ b) [] (zip [0..] rows)

flattenRow :: Int -> Row -> [(Int, Int, Piece)]
flattenRow rowIx (Row ps) = zipWith (\colIx p -> (rowIx,colIx,p)) [0..] ps

unflattenBoard :: [(Int, Int, Piece)] -> Board
unflattenBoard ps = Board ( map (\xs -> Row (map (\(_,_,p) -> p) xs)) piecesMatrix)
    where piecesMatrix = groupBy (\(r1,c1,p1) (r2,c2,p2) -> r1 == r2) ps

isGoal :: Board -> Bool
isGoal (Board rows) = all (== False) $ map (\(Row xs) -> any (== Goal) xs) rows

printValidMoves :: Board -> IO ()
printValidMoves board = mapM_ (\(b,_) -> putStr ((show b)++"\n")) (validMoves board)

printBoards :: [Board] -> IO ()
printBoards boards = mapM_ (\board -> putStr ((show board)++"\n")) boards