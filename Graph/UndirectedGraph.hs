{-# Language FlexibleInstances, FunctionalDependencies #-}

module UndirectedGraph(empty,
                       forest,
                       UndirectedGraph,
                       ) where 

import qualified Data.Set as Set
import Graph

data UndirectedGraph v e = UG {dg :: DirectedGraph v e}
                           
empty :: (Ord v, Ord e) => DirectedGraph v e
empty = DG Set.empty Set.empty

instance (Ord e, Ord v) => Graph (UndirectedGraph v e) v e where
  addVertex g v = UG (DG (Set.insert v vertices) ((edges . dg) g))
    where vertices = (verts . dg) g
  addVertices = foldl' addVertex
  removeVertex g v = UG (DG newVerts newEdges)
    where newVerts = Set.delete v ((verts . dg) g)
          newEdges = Set.filter (\e -> not $ any (== v) [to e, from e]) ((edges . dg) g)
  removeVertices = foldl' removeVertex
  addEdge g e = UG (DG vertices newEdges)
    where vertices = Set.insert (from e) $ Set.insert (to e) ((verts . dg) g)
          newEdges = Set.insert reverseEdge $ Set.insert e ((edges . dg) g)
          reverseEdge = makeEdge (from e) (to e) (val e)
  addEdges = foldl' addEdge
  removeEdge g e = UG (DG ((verts . dg) g) (Set.delete e ((edges . dg) g)))
  removeEdges = foldl' removeEdge
  getEdges = edges . dg
  getVertices = verts . dg
  inEdges g v = Set.filter ((== v) . to) ((edges . dg) g)
  outEdges g v = Set.filter ((== v) . from) ((edges . dg) g)
  forest = addVertices (UG empty)
--  new = UG Set.empty Set.empty
  fromEdges ls = addEdges (UG empty) (Set.toList ls)
  fromEdges' nl el = addEdges (forest (Set.toList nl)) (Set.toList el)
  
data DirectedGraph v e = DG {verts :: Set.Set v,
                             edges :: Set.Set (Edge v e)
                             }
                         
instance (Ord e, Ord v) => Graph (DirectedGraph v e) v e where
  addVertex g v = DG (Set.insert v vertices) (edges g)
    where vertices = verts g
  addVertices = foldl' addVertex
  removeVertex g v = DG newVerts newEdges
    where newVerts = Set.delete v (verts g)
          newEdges = Set.filter (\e -> not $ any (== v) [to e, from e]) (edges g)
  removeVertices = foldl' removeVertex
  addEdge g e = DG vertices newEdges
    where vertices = Set.insert (from e) $ Set.insert (to e) (verts g)
          newEdges = Set.insert reverseEdge $ Set.insert e (edges g)
          reverseEdge = makeEdge (from e) (to e) (val e)
  addEdges = foldl' addEdge
  removeEdge g e = DG (verts g) (Set.delete e (edges g))
  removeEdges = foldl' removeEdge
  getEdges = edges
  getVertices = verts
  inEdges g v = Set.filter ((== v) . to) (edges g)
  outEdges g v = Set.filter ((== v) . from) (edges g)
  forest = addVertices empty
--  new = DG Set.empty Set.empty
  fromEdges ls = addEdges empty (Set.toList ls)
  fromEdges' nl el = addEdges (addVertices empty (Set.toList nl)) (Set.toList el)
