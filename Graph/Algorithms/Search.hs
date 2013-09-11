module Search where

import Graph

bfsPath :: (Graph g v e) => g  -> v -> v -> [v]
bfsPath = undefined

dfsPath :: (Graph g v e) => g -> v -> v -> [v]
dfsPath = undefined

dijkstras :: (Ord e, Graph g v e) => g -> v -> v -> [v]
dijkstras = undefined
