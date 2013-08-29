module Network where

import Data.Array.Unboxed
import Data.Maybe


class Node v where
  value  :: v -> v
  idVal  :: v -> Int

--Node {value :: v, id :: Int} --TODO class declaration instead? 
class Edge e where
  to, from :: Node v => e -> v
  edge     :: e -> e

--  A network is represented as an unboxed adjacency matrix
--  found in the Data.Array.Unboxed class
data Network v e = Network {adjacencyMatrix :: Array v (Maybe e),
                            size :: Int,
                            nodes :: [v],
                            edges :: [e]
                           }

--  createForest:
--      creates a empty forest from the list of nodes, you
--      must specify a type for the edges since Haskell's 
--      type inference will not be able to determine it without
--      any edge to put in it. The edges will be filled with Nothing
--      from the Maybe monad
createForest :: (Node a, Edge e) => [a] -> Network e a
createForest = undefined

createFull ::(Node a, Edge e) => [a] -> (a -> a -> e)-> Network e a
createFull = undefined

addNode :: (Node v, Edge e) => Network v e -> v -> Network v e
addNode = undefined

removeNode :: (Node v, Edge e) => Network v e -> v -> Network v e
removeNode = undefined

addEdge :: (Node v, Edge e) => Network v e -> e -> Network v e
addEdge = undefined

removeEdge :: (Node v, Edge e) => Network v e -> e -> Network v e
removeEdge = undefined

getAdjacentNodes :: (Node v, Edge e) => Network v e -> v -> Array Int v
getAdjacentNodes = undefined

getOutEdges :: (Node v, Edge e) => Network v e -> v -> Array v e
getOutEdges = undefined

getInEdges :: (Node v, Edge e) => Network v e -> v -> Array v e
getInEdges = undefined

