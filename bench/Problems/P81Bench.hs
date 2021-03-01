module Problems.P81Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P81    as Problem
import qualified Solutions.P81   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P81"
  [ subgroup "paths" Problem.paths
  , bgroup "Solutions"
    [ subgroup "paths" Solution.paths ]
  ]

subgroup :: String -> (Vertex -> Vertex -> G -> [[Vertex]]) -> Benchmark
subgroup name paths = bgroup name
  [ bench (size g5)  $ nf (paths 1 5)  g5
  , bench (size g10) $ nf (paths 1 10) g10
  ]
  where g5  = generate (mkStdGen 12) 5
        g10 = generate (mkStdGen 23) 10

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)

size :: Graph g => g -> String
size g = "graph size " ++ show (Set.size $ vertexes g, Set.size $ edges g)
