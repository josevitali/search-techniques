{-# LANGUAGE ExistentialQuantification #-}
module Problems.Problem
(
    Problem,
    newState,
    isGoal,
    nextStates,
    heuristic,
    printStates
)
where

class Problem a where
    newState :: String -> a
    isGoal :: a -> Bool
    nextStates :: a -> [(a, Int)]
    heuristic :: a -> Int
    printStates :: Int -> Int -> [a] -> IO ()