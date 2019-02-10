module Engine.Solver
(dfsSearch,
bfsSearch)
where

import Data.Stack as Stack

import Data.Maybe

data Node a = Node { state :: a
                   , parent :: Maybe (Node a)
                   , cost :: Int
                   , depth :: Int
                   }

dfsSearch :: (Eq a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a])
dfsSearch startState isGoal nextStates
    | isGoal startState = Just (0, [startState])
    | otherwise = dfs isGoal nextStates (stackPush stackNew startNode) []
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}

dfs :: (Eq a) => (a -> Bool) -> (a -> [(a, Int)]) -> Stack (Node a) -> [a] -> Maybe (Int, [a])
dfs isGoal nextStates stack visited
    | stackIsEmpty stack = Nothing
    | isGoal nodeState = Just (nodeToList currentNode)
    | elem nodeState visited = dfs isGoal nextStates popedStack visited
    | otherwise = dfs isGoal nextStates nextStack (nodeState:visited)
        where Just (popedStack, Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}) = stackPop stack
              currentNode = Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}
              childStates = nextStates nodeState
              nextNodeList = map (\child -> Node {state=fst child, parent=Just currentNode, cost= snd child, depth=nodeDepth + 1 }) childStates
              nextStack = foldr (\node currStack -> stackPush currStack node) popedStack nextNodeList


bfsSearch :: (Eq a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a])
bfsSearch startState isGoal nextStates
    | isGoal startState = Just (0, [startState])
    | otherwise = bfs isGoal nextStates [startNode] []
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}

bfs :: (Eq a) => (a -> Bool) -> (a -> [(a, Int)]) -> [Node a] -> [a] -> Maybe (Int, [a])
bfs isGoal nextStates opened visited
    | null opened = Nothing
    | isGoal nodeState = Just (nodeToList currentNode)
    | elem nodeState visited = bfs isGoal nextStates popedOpened visited
    | otherwise = bfs isGoal nextStates nextOpened (nodeState:visited)
        where (Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}):popedOpened = opened
              currentNode = Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}
              childStates = nextStates nodeState
              nextNodeList = map (\child -> Node {state=fst child, parent=Just currentNode, cost= snd child, depth=nodeDepth + 1 }) childStates
              nextOpened = popedOpened ++ nextNodeList


nodeToList :: Node a -> (Int, [a])
nodeToList Node {state=nodeState, parent=Nothing, cost=nodeCost, depth=_} = (nodeCost, [nodeState])
nodeToList Node {state=nodeState, parent=Just nodeParent, cost=nodeCost, depth=_} = ((fst ret) + nodeCost, (snd ret) ++ [nodeState])
                                                                                where ret = nodeToList nodeParent
