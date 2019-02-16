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

getSolver :: (Eq a, Problem a) => String -> (a -> Maybe (Int, [a]))
getSolver "dfs"     = Solver.dfsSearch
getSolver "bfs"     = Solver.bfsSearch
getSolver "iddfs"    = Solver.iddfsSearch
getSolver "greedy"  = Solver.greedySearch
getSolver "astar"   = Solver.aStarSearch

start :: String -> String -> String -> IO ()
start searchType "maze" fileText = printSolution (getSolver searchType (newState fileText :: Board.Board))
start searchType "lala" fileText = printSolution (getSolver searchType (newState fileText :: Lala.Lala))
start searchType _      fileText = problemError >> usage >> exit

printSolution :: (Problem a) => Maybe(Int, [a]) -> IO ()
printSolution Nothing            = putStrLn "Solution not found"
printSolution (Just(cost, path)) = printStates cost path
