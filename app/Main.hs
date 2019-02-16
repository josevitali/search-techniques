module Main where

import qualified    Problems.Board     as Board
import qualified    Problems.Lala      as Lala
import              Engine.Solver      as Solver
import              Problems.Problem   as Problem
import              Application        as App

import              Data.Maybe
import              System.Environment
import qualified    System.Exit        as E
import              System.IO          as IO
import              Data.List.Split

main :: IO ()
main = getArgs >>= parse

parse ["-h"]                                    = usage >> exit
parse ["-help"]                                 = usage >> exit
parse ["-v"]                                    = version >> exit
parse ["--version"]                             = version >> exit
parse [searchType, problem, pathFile]           = openFile pathFile ReadWriteMode
                                                    >>= hGetContents
                                                    >>= (\text -> start searchType problem text)
parse _                                         = usage >> die

usage = putStrLn "Usage: search-techniques <search_type> <problem> <file_path>"

searchTypeError = putStrLn "Parse error: invalid search_type"

problemError = putStrLn "Parse error: invalid problem"

version = putStrLn "search-techniques 1.0"

exit = E.exitWith E.ExitSuccess

die = E.exitWith (E.ExitFailure 1)

solverSearch :: (Problem a) => a -> (a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a])) -> Maybe (Int, [a])
solverSearch problem solver = solver problem isGoal nextStates

getSolver :: (Eq a, Problem a) => String -> (a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a]))
getSolver "dfs"     = Solver.dfsSearch
getSolver "bfs"     = Solver.bfsSearch
getSolver "iddf"    = Solver.iddfsSearch 0
getSolver "greedy"  = Solver.greedySearch heuristic
getSolver "astar"   = Solver.aStarSearch heuristic

start :: String -> String -> String -> IO ()
start searchType "maze" fileText = printSolution (solverSearch (newState fileText :: Board.Board) $ getSolver searchType)
start searchType "lala" fileText = printSolution (solverSearch (newState fileText :: Lala.Lala) $ getSolver searchType)
start searchType _      fileText = problemError >> usage >> exit

printSolution :: (Problem a) => Maybe(Int, [a]) -> IO ()
printSolution Nothing            = putStrLn "Solution not found"
printSolution (Just(cost, path)) = printStates cost path