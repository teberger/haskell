{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Network(Vertex, 
               v, 
               iden, 
               Edge, 
               to, 
               from, 
               val, 
{-               Graph, 
               vertices, 
               edges,
               addVertex,
               removeVertex,
               addVertices,
               removeVertices,
               addEdge,
               removeEdge,
               addEdge',
               removeEdge',
               addEdges,
               removeEdges-}) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

data Vertex a = V {v :: a,
                   iden :: Int
                  } deriving Show
                
instance Eq (Vertex a) where 
  a == b = (iden a) == (iden b)
  
instance Ord(Vertex a) where
  a < b = (iden a) < (iden b)

data Edge a b = (Eq b) => Edge {to   :: Vertex a, 
                                from :: Vertex a,
                                val  :: b
                               }

instance (Eq b, Eq a) => Eq (Edge a b) where                
  (==) a b = and [(to a) == (to b), (from a) == (from b)]
  
{-
data Graph v e = Directed {vertices :: Set.Set (Vertex v),
                           edges    :: Set.Set (Edge v e)
                          }

addVertex :: Graph v e -> Vertex v -> Graph v e
addVertex = undefined

removeVertex :: Graph v e -> Vertex v -> Graph v e
removeVertex = undefined

addVertices :: Graph v e -> [Vertex v] -> Graph v e
addVertices = undefined

removeVertices :: Graph v e -> [Vertex v] -> Graph v e
removeVertices = undefined

addEdge :: Graph v e -> Vertex v -> Vertex v -> b -> Graph v e
addEdge = undefined

removeEdge :: Graph v e -> Vertex v -> Vertex v -> Graph v e
removeEdge = undefined

addEdge' :: Graph v e -> Edge v e -> Graph v e
addEdge' = undefined

removeEdge' :: Graph v e -> Edge v e -> Graph v e
removeEdge' = undefined

addEdges :: Graph v e -> [Edge v e] -> Graph v e
addEdges = undefined

removeEdges :: Graph v e -> [Edge v e] -> Graph v e
removeEdges = undefined

mapVert :: Graph v e -> (Vertex v -> Vertex c) -> Graph c b
mapVert = 
G
mapEdge :: Graph v e -> (Edge v e -> Edge a c) -> Graph a c
mapEdge = undefined
-}

class Graph g v e | g -> v e where
  addVertex :: g -> v -> g
  addEdge   :: g -> e -> g
  
  
data UndirectedGraph v e = UG {verts :: Set.Set v,
                               edges :: Set.Set (Edge v e)
                              }
                           
instance (Ord v) => Graph (UndirectedGraph v e) v e where
  addVertex g v = UG (Set.insert v (verts g)) (edges g)
  addEdge = undefined