module Main where

import Lib
import Problems.Board
import Engine.Solver

import Data.Maybe

main :: IO ()
main = do { let board = Problems.Board.newBoard [['.','.','.', 'p'],
                                                ['.','.','w','.'],
                                                ['w','.','.','w'],
                                                ['.','.','.','g']]
        --   ; putStrLn (show board) }
          -- ; putStrLn (show $ dfsSearch board isGoal validMoves) }
          ; let (cost, search) = fromJust (iddfsSearch board isGoal validMoves 0)
        --   ; let search = fromJust (dfsSearch board isGoal validMoves)
          ; printBoards search
          ; putStrLn (show cost) }