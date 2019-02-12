module Engine.Solver
(dfsSearch,
bfsSearch,
aStarSearch)
where

import Data.Stack as Stack
import qualified Data.PQueue.Prio.Min as PQ
import Data.Maybe

data SolverCollection a = PQueue (PQ.MinPQueue Int (Node a)) (a -> Int) -- PQ and heuristic
                            | Stack (Stack (Node a))
                            | Queue [Node a]

data Node a = Node { state :: a
                   , parent :: Maybe (Node a)
                   , cost :: Int
                   , depth :: Float
                   }


infinity = (1/0)


dfsSearch :: (Eq a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a])
dfsSearch startState isGoal nextStates = solve isGoal nextStates (Stack (stackPush stackNew startNode)) [] infinity
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


bfsSearch :: (Eq a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> Maybe (Int, [a])
bfsSearch startState isGoal nextStates = solve isGoal nextStates (Queue [startNode]) [] infinity
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


aStarSearch :: (Eq a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
aStarSearch startState isGoal nextStates heuristic = solve isGoal nextStates (PQueue (PQ.singleton (heuristic startState) startNode) heuristic) [] infinity
                where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


solve :: (Eq a) => (a -> Bool) -> (a -> [(a, Int)]) -> SolverCollection a -> [a] -> Float -> Maybe (Int, [a])
solve isGoal nextStates opened visited maxDepth
            -- if open nodes is empty, path not found
            | isCollectionEmpty opened = Nothing
            -- if current state already visited or max depth reached, skip and keep searching
            | elem nodeState visited || nodeDepth > maxDepth = solve isGoal nextStates popedOpened visited maxDepth
            -- if current state is Goal, return found path
            | isGoal nodeState = Just (nodeCost, nodeToList currentNode)
            -- otherwise explode current state and add child states to open nodes
            | otherwise = solve isGoal nextStates nextOpened (nodeState:visited) maxDepth
                    where 
                        -- get next node in open nodes
                        Just (popedOpened, Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}) = collectionPop opened
                        currentNode = Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth}
                        -- get child states from current state
                        childStates = nextStates nodeState
                        -- add child nodes to open nodes
                        nextNodeList = map (\(childState, childCost) -> Node {state=childState, parent=Just currentNode, cost= childCost + nodeCost, depth=nodeDepth + 1 }) childStates
                        nextOpened = addAllToCollection nextNodeList popedOpened


isCollectionEmpty :: SolverCollection a -> Bool
isCollectionEmpty (PQueue pq heuristic) = PQ.null pq
isCollectionEmpty (Stack stack) = stackIsEmpty stack
isCollectionEmpty (Queue queue) = null queue


collectionPop :: SolverCollection a -> Maybe(SolverCollection a, Node a)
collectionPop (PQueue pq heuristic) = PQ.getMin pq >>= (\(_, node) -> Just(PQueue (PQ.deleteMin pq) heuristic, node))
collectionPop (Stack stack) = stackPop stack >>= (\(popedStack, node) -> Just(Stack popedStack, node))
collectionPop (Queue []) = Nothing
collectionPop (Queue (node:popedOpened)) = Just(Queue popedOpened, node)


addAllToCollection :: [Node a] -> SolverCollection a -> SolverCollection a
addAllToCollection nextNodeList (PQueue pq heuristic) = PQueue (foldr (\node currOpened -> addToPQ node currOpened heuristic) pq nextNodeList) heuristic
addAllToCollection nextNodeList (Stack stack) = Stack (foldr (\node currStack -> stackPush currStack node) stack nextNodeList)
addAllToCollection nextNodeList (Queue queue) = Queue $ queue ++ nextNodeList


addToPQ :: Node a -> PQ.MinPQueue Int (Node a) -> (a -> Int) -> PQ.MinPQueue Int (Node a)
addToPQ node pq heuristic = PQ.insert (cost nodeParent + heuristic nodeState) node pq
                                where Node {state=nodeState, parent=Just nodeParent, cost=nodeCost, depth=nodeDepth} = node 


nodeToList :: Node a -> [a]
nodeToList Node {state=nodeState, parent=Nothing, cost=nodeCost, depth=_} = [nodeState]
nodeToList Node {state=nodeState, parent=Just nodeParent, cost=nodeCost, depth=_} = nodeToList nodeParent ++ [nodeState]
