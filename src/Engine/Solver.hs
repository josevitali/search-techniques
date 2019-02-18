module Engine.Solver
(dfsSearch,
depthLimSearch,
iddfsSearch,
bfsSearch,
aStarSearch,
greedySearch)
where

import           Problems.Problem
import           Data.Stack             as Stack
import qualified Data.PQueue.Prio.Min   as PQ
import           Data.Maybe

data SolverCollection a = PQueue (PQ.MinPQueue Int (Node a)) (Int -> a -> Int) -- PQ and priority function
                            | Stack (Stack (Node a))
                            | Queue [Node a]

data Node a = Node { state :: a
                   , parent :: Maybe (Node a)
                   , cost :: Int
                   , depth :: Float
                   }


infinity = (1/0)


dfsSearch :: (Eq a, Problem a) => a -> Maybe (Int, [a])
dfsSearch startState = solve (Stack (stackPush stackNew startNode)) [] infinity
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


iddfsSearch :: (Eq a, Problem a) => a -> Maybe (Int, [a])
iddfsSearch startState = solveIddfs 0 startState

solveIddfs :: (Eq a, Problem a) => Float -> a -> Maybe (Int, [a])
solveIddfs depth startState
            | isJust currCall = currCall
            | otherwise = solveIddfs (depth+1) startState
                where
                    currCall = depthLimSearch depth startState


depthLimSearch :: (Eq a, Problem a) => Float -> a -> Maybe (Int, [a])
depthLimSearch maxDepth startState = solve (Stack (stackPush stackNew startNode)) [] maxDepth
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


bfsSearch :: (Eq a, Problem a) => a -> Maybe (Int, [a])
bfsSearch startState = solve (Queue [startNode]) [] infinity
        where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


aStarSearch :: (Eq a, Problem a) => a -> Maybe (Int, [a])
aStarSearch startState = solve (PQueue (PQ.singleton (heuristic startState) startNode) aStarPriority) [] infinity
                where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


greedySearch :: (Eq a, Problem a) => a -> Maybe (Int, [a])
greedySearch startState = solve (PQueue (PQ.singleton (heuristic startState) startNode) greedyPriority) [] infinity
                where startNode = Node {state=startState, parent=Nothing, cost=0, depth=0}


solve :: (Eq a, Problem a) => SolverCollection a -> [a] -> Float -> Maybe (Int, [a])
solve opened visited maxDepth
            -- if open nodes is empty, path not found
            | isCollectionEmpty opened = Nothing
            -- if current state already visited skip and keep searching
            | elem (nodeState) visited = solve popedOpened visited maxDepth
            -- if max depth reached skip and keep searching
            | nodeDepth > maxDepth = solve popedOpened ((nodeState):visited) maxDepth
            -- if current state is Goal, return found path
            | isGoal nodeState = Just (nodeCost, nodeToList currentNode)
            -- otherwise explode current state and add child states to open nodes
            | otherwise = solve nextOpened ((nodeState):visited) maxDepth
                    where 
                        -- get next node in open nodes
                        Just (popedOpened, currentNode) = collectionPop opened
                        Node {state=nodeState, parent=nodeParent, cost=nodeCost, depth=nodeDepth} = currentNode
                        -- get child states from current state
                        childStates = nextStates nodeState
                        -- add child nodes to open nodes
                        nextNodeList = map (\(childState, childCost) -> Node {state=childState, parent=Just currentNode,
                         cost= childCost + nodeCost, depth=nodeDepth + 1 }) childStates
                        nextOpened = addAllToCollection nextNodeList popedOpened


isCollectionEmpty :: SolverCollection a -> Bool
isCollectionEmpty (PQueue pq heuristic) = PQ.null pq
isCollectionEmpty (Stack stack) = stackIsEmpty stack
isCollectionEmpty (Queue queue) = null queue


collectionPop :: SolverCollection a -> Maybe(SolverCollection a, Node a)
collectionPop (PQueue pq priority) = PQ.getMin pq >>= (\(_, node) -> Just(PQueue (PQ.deleteMin pq) priority, node))
collectionPop (Stack stack) = stackPop stack >>= (\(popedStack, node) -> Just(Stack popedStack, node))
collectionPop (Queue []) = Nothing
collectionPop (Queue (node:popedOpened)) = Just(Queue popedOpened, node)


addAllToCollection :: [Node a] -> SolverCollection a -> SolverCollection a
addAllToCollection nextNodeList (PQueue pq priority) = PQueue (foldr (\node currOpened -> addToPQ node currOpened priority) pq nextNodeList) priority
addAllToCollection nextNodeList (Stack stack) = Stack (foldl (\currStack node -> stackPush currStack node) stack nextNodeList)
addAllToCollection nextNodeList (Queue queue) = Queue $ queue ++ nextNodeList


addToPQ :: Node a -> PQ.MinPQueue Int (Node a) -> (Int -> a -> Int) -> PQ.MinPQueue Int (Node a)
addToPQ node pq priority = PQ.insert (priority nodeCost nodeState) node pq
                                where Node {state=nodeState, parent=Just nodeParent, cost=nodeCost, depth=nodeDepth} = node 


aStarPriority :: (Problem a) => Int -> a -> Int
aStarPriority cost state = cost + heuristic state


greedyPriority :: (Problem a) => Int -> a -> Int
greedyPriority _ state = heuristic state


nodeToList :: Node a -> [a]
nodeToList Node {state=nodeState, parent=Nothing, cost=nodeCost, depth=_} = [nodeState]
nodeToList Node {state=nodeState, parent=Just nodeParent, cost=nodeCost, depth=_} = nodeToList nodeParent ++ [nodeState]
