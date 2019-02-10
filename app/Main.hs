module Main where

import Lib
import Problems.Board
import Engine.Solver

import Data.Maybe

main :: IO ()
main = do { let board = Problems.Board.newBoard [['.','.','.', 'p'],
                                                ['.','.','w','.'],
                                                ['.','w','.','w'],
                                                ['.','.','g','.']]
        --   ; putStrLn (show board) }
          -- ; putStrLn (show $ dfsSearch board isGoal validMoves) }
          ; let search = fromJust (bfsSearch board isGoal validMoves)
          ; printBoards $ snd (search)
          ; putStrLn (show (fst search)) }