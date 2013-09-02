{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Network(Edge, 
               to, 
               from, 
               val, 
               ) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

data Vertex a = V {v :: a,
                   iden :: Int
                  } deriving Show
                
instance Eq (Vertex a) where 
  a == b = (iden a) == (iden b)
  
instance Ord (Vertex a) where
  a < b = (iden a) < (iden b)

data Edge a b = (Eq b) => Edge {to   :: a, 
                                from :: a,
                                val  :: b
                               }

instance (Eq b, Eq a) => Eq (Edge a b) where                
  (==) a b = (to a) == (to b) && (from a) == (from b)
  
instance (Ord v, Eq e) => (Ord (Edge v e)) where
  e1 < e2 = (to e1) < (to e2)
  e1 >= e2 = (to e1) >= (to e2)
  e1 > e2 = (to e1) > (to e2)
  e1 <= e2 = (to e1) <= (to e2)
  
instance (Show v, Show e) => Show (Edge v e) where
  show e = "//Edge to: " ++ (show $ to e) ++ ", Edge from: " ++ (show $ from e) ++ ", val: " ++ (show $ val e) ++ "//"

class Graph g v e | g -> v e where
  addVertex      :: g ->  v -> g
  addVertices    :: g -> [v] -> g
  removeVertex   :: g ->  v  -> g
  removeVertices :: g -> [v] -> g
  addEdge        :: g ->  Edge v e  -> g
  addEdges       :: g -> [Edge v e] -> g
  removeEdge     :: g ->  Edge v e  -> g
  removeEdges    :: g -> [Edge v e] -> g
  getVertices    :: g -> Set.Set v
  getEdges       :: g -> Set.Set (Edge v e)
  outEdges       :: g -> v -> Set.Set (Edge v e)
  inEdges        :: g -> v -> Set.Set (Edge v e)
  

data UndirectedGraph v e = UG {verts :: Set.Set v,
                               edges :: Set.Set (Edge v e)
                              }
                           
instance (Ord e, Ord v) => Graph (UndirectedGraph v e) v e where
  addVertex g v = UG (Set.insert v vertices) (edges g)
    where vertices = verts g
  addVertices = foldl' addVertex
  removeVertex g v = UG newVerts newEdges
    where newVerts = Set.delete v (verts g)
          newEdges = Set.filter (\e -> not $ any (== v) [to e, from e]) (edges g)
  removeVertices = foldl' removeVertex
  addEdge g e = UG vertices newEdges
    where vertices = Set.insert (from e) $ Set.insert (to e) (verts g)
          newEdges = Set.insert reverseEdge $ Set.insert e (edges g)
          reverseEdge = Edge (from e) (to e) (val e)
  addEdges = foldl' addEdge
  removeEdge g e = UG (verts g) (Set.delete e (edges g))
  removeEdges = foldl' removeEdge
  getEdges = edges
  getVertices = verts