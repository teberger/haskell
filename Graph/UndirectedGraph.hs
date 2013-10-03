{-# Language FlexibleInstances, FunctionalDependencies #-}

module UndirectedGraph(empty,
                       forest,
                       UndirectedGraph,
                       ) where 

import qualified Data.Set as Set
import Graph
import DirectedGraph

data UndirectedGraph v e = UG {dg :: DirectedGraph v e}
                           
empty :: (Ord v, Ord e) => DirectedGraph v e
empty = DG Set.empty Set.empty

instance (Ord e, Ord v) => Graph (UndirectedGraph v e) v e where
  addEdge g e = UG (DG vertices newEdges)
     where vertices = Set.insert (from e) $ Set.insert (to e) (getVertices g)
           newEdges = Set.insert reverseEdge $ Set.insert e (getEdges g)
           reverseEdge = makeEdge (from e) (to e) (val e)
  removeEdge g e = UG (DG (getVertices g) newEdges)
     where newEdges = Set.delete reverseEdge $ Set.delete e (getEdges g)
           reverseEdge = makeEdge (from e) (to e) (val e)
  getEdges = edges . dg  
  getVertices = verts . dg
  fromEdges ls = addEdges (UG empty) (Set.toList ls)
  newGraph nl el = addEdges (forest (Set.toList nl)) (Set.toList el)
  
