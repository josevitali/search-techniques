module Problems.Maze
(
    Maze
) where

import Problems.Problem

import Data.List
import Data.List.Split
import Data.Maybe

data Row = Row [Piece] deriving (Eq)

data Maze = Maze [Row] deriving (Eq)

data Piece = Player | Goal | EmptySpace | Wall deriving(Eq, Enum)

instance Show Piece where
    show Player = "p"
    show Goal = "g"
    show EmptySpace = "."
    show Wall = "w"

instance Show Row where
    show (Row row) = foldr (\piece b -> (show piece)++"\t"++b) "\n" row

instance Show Maze where
    show (Maze rows) = foldr (\row b -> (show row)++b) "" rows

instance Read Maze where
    readsPrec _ text = [(newMaze text, text)]

instance Problem Maze where
    newState = newMaze
    isGoal = isMazeGoal
    nextStates = validMoves
    heuristic = mazeHeuristic
    printStates = printMazes

newMaze :: String -> Maze
newMaze rows = Maze $ map (\row -> Row $ map newPiece row) (splitOn "\n" rows)

newPiece :: Char -> Piece
newPiece 'p' = Player
newPiece 'g' = Goal
newPiece '.' = EmptySpace
newPiece 'w' = Wall
newPiece p = error "not a valid piece in maze"

moveCost = 1

validMoves :: Maze -> [(Maze, Int)]
validMoves maze = 
    map fromJust $ filter isJust possibleMoves
    where
        possibleMoves = map (\(r,c,p) -> if isValidMove (r,c,p) maze then movePlayer maze (r,c) >>= (\b -> Just (b,moveCost)) else Nothing) (flattenMaze maze)

movePlayer :: Maze -> (Int, Int) -> Maybe Maze
movePlayer maze (row, col) =
    playerPosition maze >>= (\(prow, pcol, _) -> movePiece (prow, pcol, EmptySpace) maze) >>= (\b -> movePiece (row, col, Player) (unflattenMaze b)) >>=
        (\ps -> Just (unflattenMaze ps))

movePiece :: (Int, Int, Piece) -> Maze -> Maybe [(Int, Int, Piece)]
movePiece (row, col, piece) maze = findIndex (\(r,c,_) -> r == row && c == col) (flattenMaze maze) >>=
    (\index -> Just (splitAt (index) (flattenMaze maze)) ) >>=
    (\(xs,_:ys) -> Just ( xs++[(row,col,piece)]++ys) )


isValidMove :: (Int, Int, Piece) -> Maze -> Bool
isValidMove (_, _, Player) _ = False
isValidMove (_, _, Wall) _ = False
isValidMove (row, col, _) maze = rowDistance <= 1 && colDistance <= 1 && rowDistance + colDistance == 1
    where
        (prow, pcol, _) = fromJust $ playerPosition maze
        rowDistance = (abs $ row - prow)
        colDistance = (abs $ col - pcol)

mazeHeuristic :: Maze -> Int
mazeHeuristic maze = case goalPosition maze of
    Nothing -> 0
    Just (grow, gcol, _) -> (abs $ prow - grow) + (abs $ pcol - gcol)
        where
            (prow, pcol, _) = fromJust $ playerPosition maze

playerPosition :: Maze -> Maybe (Int, Int, Piece)
playerPosition maze = piecePosition maze Player

goalPosition :: Maze -> Maybe (Int, Int, Piece)
goalPosition maze = piecePosition maze Goal

piecePosition :: Maze -> Piece -> Maybe (Int, Int, Piece)
piecePosition maze piece = let ps = flattenMaze maze in do
    ix <- findIndex (\(_,_,p) -> p == piece) ps
    Just (ps!!ix)

flattenMaze :: Maze -> [(Int, Int, Piece)]
flattenMaze (Maze rows) = foldr (\indexedRow b -> (flattenRow (fst indexedRow) (snd indexedRow)) ++ b) [] (zip [0..] rows)

flattenRow :: Int -> Row -> [(Int, Int, Piece)]
flattenRow rowIx (Row ps) = zipWith (\colIx p -> (rowIx,colIx,p)) [0..] ps

unflattenMaze :: [(Int, Int, Piece)] -> Maze
unflattenMaze ps = Maze ( map (\xs -> Row (map (\(_,_,p) -> p) xs)) piecesMatrix)
    where piecesMatrix = groupBy (\(r1,c1,p1) (r2,c2,p2) -> r1 == r2) ps

isMazeGoal :: Maze -> Bool
isMazeGoal (Maze rows) = all (== False) $ map (\(Row xs) -> any (== Goal) xs) rows

printValidMoves :: Maze -> IO ()
printValidMoves maze = mapM_ (\(b,_) -> putStr ((show b)++"\n")) (validMoves maze)

printMazes :: Int -> Int-> [Maze] -> IO ()
printMazes visitedLength cost mazes = mapM_ (\maze -> do
                                                        clearScreenOnDemand
                                                        putStr ((show maze)++"\n")
         ) mazes >> putStrLn (concat ["Total cost: ", (show cost)])
                                                                            >> putStrLn (concat ["Visited states: ", (show visitedLength)])

clearScreenOnDemand :: IO ()
clearScreenOnDemand = do getChar;putStr "\ESC[2J"