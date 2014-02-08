module Search where

import Graph

bfsPath :: (Graph g v e) => g -> v -> v -> Maybe [v]
bfsPath graph to from = undefined

dfsPath :: (Graph g v e) => g -> v -> v -> Maybe [v]
dfsPath = undefined

dijkstras :: (Ord e, Graph g v e) => g -> v -> v -> Maybe [v]
dijkstras = undefined

--fast functional queue for bfs and dijkstras
data Queue a = Queue {top :: [a], 
                      back :: [a]
                     }

makeQueue :: Queue a
makeQueue = Queue [] []

push :: Queue a -> a -> Queue a
push q e = Queue (top q) (e:(back q))


pushAll :: Queue a -> [a] -> Queue a
pushAll q = foldr (flip push) q

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue [] as) = (Just (head as'), Queue as [])
  where as' = reverse as
pop (Queue xs as) = (Just . head $ xs, Queue (tail xs) as)


elemQ :: Eq a => a -> Queue a -> Bool
elemQ a q = or [a `elem` (top q), a `elem` (back q)]
