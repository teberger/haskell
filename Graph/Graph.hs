{-# LANGUAGE GADTs, FunctionalDependencies, FlexibleInstances #-}
module Graph(Edge, 
             makeEdge,
             to, 
             from, 
             val,
             Graph,
             addVertex,
             addVertices,
             removeVertex,
             removeVertices,
             addEdge,
             addEdges,  
             removeEdge,
             removeEdges,
             getVertices,
             getEdges,
             outEdges,
             inEdges,        
            module Data.List,
            ) where

import Data.List
import qualified Data.Set as Set


-- Edge data constructor and instance delcarations
data Edge a b = (Eq b) => Edge {to   :: a, 
                                from :: a,
                                val  :: b
                               }
                
makeEdge :: (Eq a, Eq b) => a -> a -> b -> Edge a b
makeEdge t f v = Edge t f v
  
instance (Eq b, Eq a) => Eq (Edge a b) where                
  (==) a b = (to a) == (to b) && (from a) == (from b) && (val a) == (val b)
  
instance (Ord v, Eq e) => (Ord (Edge v e)) where
  e1 < e2 = (to e1) < (to e2)
  e1 >= e2 = (to e1) >= (to e2)
  e1 > e2 = (to e1) > (to e2)
  e1 <= e2 = (to e1) <= (to e2)
  
instance (Show v, Show e) => Show (Edge v e) where
  show e = " [" ++ (show $ val e) ++ "]={" ++ (show $ to e) ++ "}->{" ++ (show $ from e) ++ "}"


-- Graph class starts here
class Graph g v e | g -> v e where
  addVertex, removeVertex     :: g ->  v -> g
  addVertices, removeVertices :: g -> [v] -> g
  addEdge, removeEdge         :: g ->  Edge v e  -> g
  addEdges, removeEdges       :: g -> [Edge v e] -> g
  getVertices                 :: g -> Set.Set v
  getEdges                    :: g -> Set.Set (Edge v e)
  inEdges, outEdges           :: g -> v -> Set.Set (Edge v e)


toGML :: (Graph g v e) => g ->  String
toGML = undefined

{-data UndirectedGraph v e = UG {verts :: Set.Set v,
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
  inEdges g v = Set.filter ((== v) . to) (edges g)
  outEdges g v = Set.filter ((== v) . from) (edges g)-}