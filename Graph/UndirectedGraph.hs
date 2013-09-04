{-# Language FlexibleInstances, FunctionalDependencies #-}

module UndirectedGraph(empty,
                       forest,
                       UndirectedGraph,
                       ) where 

import qualified Data.Set as Set
import Graph

data UndirectedGraph v e = UG {verts :: Set.Set v,
                               edges :: Set.Set (Edge v e)
                              }
                           
empty :: (Ord v, Ord e) => UndirectedGraph v e                           
empty = UG Set.empty Set.empty

forest :: (Ord v, Ord e) => [v] -> UndirectedGraph v e
forest ls = addVertices empty ls
                           
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
          reverseEdge = makeEdge (from e) (to e) (val e)
  addEdges = foldl' addEdge
  removeEdge g e = UG (verts g) (Set.delete e (edges g))
  removeEdges = foldl' removeEdge
  getEdges = edges
  getVertices = verts
  inEdges g v = Set.filter ((== v) . to) (edges g)
  outEdges g v = Set.filter ((== v) . from) (edges g)
  
