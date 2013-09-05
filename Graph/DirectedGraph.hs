{-# Language FunctionalDependencies, FlexibleInstances #-}

module DirectedGraph where 

import Graph
import qualified Data.Set as Set

data DirectedGraph v e = DG {verts :: Set.Set v,
                             edges :: Set.Set (Edge v e)
                             }
                         
instance (Ord e, Ord v) => Graph (DirectedGraph v e) v e where
  getEdges = edges
  getVertices = verts
  fromEdges ls = addEdges (DG Set.empty Set.empty) (Set.toList ls)
  newGraph nl el = addEdges (addVertices (DG Set.empty Set.empty) (Set.toList nl)) (Set.toList el)  
                             