-- | Implementations of Infinite Games Lecture Notes https://finkbeiner.groups.cispa.de/teaching/infinite-games-16/lecture-notes.pdf

module InfiniteGames where

import Data.Set
import qualified Data.Set as Set
import Data.GraphViz.Types.Graph
import Data.Text (Text)
import qualified Data.Text as Text
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz (PrintDot(toDot))
import System.Process (createProcess, proc)
import qualified Data.Text.Lazy.IO as TextIO

data PlayerVertex _V0 _V1 = Player0Vertex _V0 | Player1Vertex _V1 deriving (Show, Eq, Ord)

-- Definition 2.1 (Arena) A = (V, V_0, V_1, E)
-- Where V_0 and V_1 are disjoint subsets of V and V is the set of all vertices
-- Every vertice has at least 1 outgoing edge
data Arena _V0 _V1 = Arena {
  _V :: Set (PlayerVertex _V0 _V1),
  _V0 :: Set _V0,
  _V1 :: Set _V1,
  _E :: Set (PlayerVertex _V0 _V1, PlayerVertex _V0 _V1)
} deriving (Show, Eq, Ord)

-- Definition 2.3 (Play) A play in an arena is an infinite sequence of vertices rho = v_0, v_1, ... \in V^\omega
type Rho _V0 _V1 = [PlayerVertex _V0 _V1]


-- Examples --

-- Figure 2.1
data ExamplePlayer0Vertex = V1 | V3 | V7 | V8 deriving (Show, Eq, Ord, Bounded, Enum)
data ExamplePlayer1Vertex = V0 | V2 | V4 | V5 | V6 deriving (Show, Eq, Ord, Bounded, Enum)

exampleArena :: Arena ExamplePlayer0Vertex ExamplePlayer1Vertex
exampleArena = Arena {
  _V = Set.map Player0Vertex (sumTypeToSet @ExamplePlayer0Vertex) `union` Set.map Player1Vertex (sumTypeToSet @ExamplePlayer1Vertex),
  _V0 = sumTypeToSet @ExamplePlayer0Vertex,
  _V1 = sumTypeToSet @ExamplePlayer1Vertex,
  _E = Set.fromList [
    -- Edges from V0
    (Player1Vertex V0, Player0Vertex V1),
    (Player1Vertex V0, Player0Vertex V3),

    -- Edges from V1
    (Player0Vertex V1, Player1Vertex V0),
    (Player0Vertex V1, Player1Vertex V2),

    -- Edges from V2
    (Player1Vertex V2, Player0Vertex V1),
    (Player1Vertex V2, Player1Vertex V5),

    -- Edges from V3
    (Player0Vertex V3, Player1Vertex V4),
    (Player0Vertex V3, Player1Vertex V6),

    -- Edges from V4
    (Player1Vertex V4, Player1Vertex V0),
    (Player1Vertex V4, Player0Vertex V7),

    -- Edges from V5
    (Player1Vertex V5, Player0Vertex V1),
    (Player1Vertex V5, Player0Vertex V7),

    -- Edges from V6
    (Player1Vertex V6, Player0Vertex V7),

    -- Edges from V7
    (Player0Vertex V7, Player1Vertex V6),
    (Player0Vertex V7, Player0Vertex V8),

    -- Edges from V8
    (Player0Vertex V8, Player1Vertex V5)
  ]
}

-- Example reachability set v_4 and V_5
_R :: Set (PlayerVertex ExamplePlayer0Vertex ExamplePlayer1Vertex)
_R = fromList [Player1Vertex V4, Player1Vertex V5]

-- Turn a sum type into a Set of all possible values
sumTypeToSet :: (Ord a, Enum a, Bounded a) => Set a
sumTypeToSet = Set.fromList [minBound .. maxBound]

arenaToDot :: (Show _V0, Show _V1) => Arena _V0 _V1 -> DotGraph Text
arenaToDot (Arena _V _V0 _V1 _E) = mkGraph
  (Set.toList $ Set.map (\v -> DotNode (Text.pack $ show v) []) _V)
  (Set.toList $ Set.map (\(v1, v2) -> DotEdge (Text.pack $ show v1) (Text.pack $ show v2) []) _E)

makeDotFile :: IO ()
makeDotFile = do
  TextIO.writeFile "./arena.dot" ( renderDot $ toDot $  arenaToDot exampleArena)
  putStrLn "Dot file created at arena.dot. Attempting to render with graphviz..."
  _ <- createProcess (proc "dot" ["-Tpng", "arena.dot", "-o", "arena.png"])
  putStrLn "Graphviz rendered arena.png. Attempting to open the file with default viewer..."
  _ <- createProcess (proc "open" ["./arena.png"])
  pure ()
