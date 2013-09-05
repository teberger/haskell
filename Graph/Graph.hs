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
             fromEdges,
             newGraph,
             toSimpleGML,
             forest,
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
  show e = " [" ++ (show $ val e) ++ "]={" ++ (show $ from e) ++ "}->{" ++ (show $ to e) ++ "}"


-- Graph class starts here
class (Ord v, Ord e) => Graph g v e | g -> v e where
  addVertex, removeVertex     :: g ->  v -> g
  addVertex g v               = newGraph (Set.insert v (getVertices g)) (getEdges g)
  removeVertex g v            = newGraph (Set.delete v (getVertices g)) newEdges
    where newEdges = Set.filter (\e -> not $ any (== v) [to e, from e]) (getEdges g)
  addVertices, removeVertices :: g -> [v] -> g
  addVertices                 = foldl' addVertex
  removeVertices              = foldl' removeVertex
  addEdge, removeEdge         :: g ->  Edge v e  -> g
  addEdge g e                 = newGraph newVerts newEdges
    where newVerts = Set.insert (from e) $ Set.insert (to e) (getVertices g)
          newEdges = Set.insert e (getEdges g)
  removeEdge g e              = newGraph (getVertices g) (Set.delete e (getEdges g))
  addEdges, removeEdges       :: g -> [Edge v e] -> g
  addEdges                    = foldl' addEdge
  removeEdges                 = foldl' removeEdge
  getVertices                 :: g -> Set.Set v
  getEdges                    :: g -> Set.Set (Edge v e)
  inEdges, outEdges           :: g -> v -> Set.Set (Edge v e)
  inEdges g v                 = Set.filter ((== v) . to) (getEdges g)
  outEdges g v                = Set.filter ((== v) . from) (getEdges g)
  fromEdges                   :: Set.Set (Edge v e) -> g
  newGraph                  :: Set.Set v -> Set.Set (Edge v e) -> g
  forest                      :: [v] -> g
  forest                      = addVertices (newGraph Set.empty Set.empty)

toSimpleGML :: (Show v, Show e, Graph g v e) => g ->  String
toSimpleGML g = "<graphml>\n" ++
          "  <graph>\n" ++
               vertsInML (getVertices g) ++ "\n" ++
               edgesInML (getEdges g) ++
          "  </graph>\n</graphml>\n"
             
  where vertsInML :: (Show v) => Set.Set v -> String
        vertsInML = concatMap (\v -> "    <node id=\"" ++ (show v) ++ "\"/>\n") . (Set.toList)
        edgesInML :: (Show e, Show v) => Set.Set (Edge v e) -> String
        edgesInML = concatMap (\e -> "    <edge source=\"" ++ (show (from e)) ++ "\" target=\"" ++ (show (to e)) ++ "\"/>\n") . (Set.toList)

mapGraph :: (Ord v, Ord e, Ord d, Ord u, Graph g v e, Graph a u d) => 
            (Edge v e -> Edge u d) -> g -> a
mapGraph f = fromEdges . ((Set.map f) . getEdges)

