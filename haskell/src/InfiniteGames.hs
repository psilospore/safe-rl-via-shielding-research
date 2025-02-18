-- | Implementations of Infinite Games Lecture Notes https://finkbeiner.groups.cispa.de/teaching/infinite-games-16/lecture-notes.pdf
module InfiniteGames where

import Data.GraphViz ( PrintDot(toDot), filled )
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Graph
import Data.Set
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as TextIO
import System.Process (createProcess, proc)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bifunctor (Bifunctor(..))
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes (X11Color(..))
import qualified Debug.Trace as Debug

debug :: Bool
debug = True

trace :: String -> a -> a
trace str = if debug then Debug.trace str else id

-- From Infinite Games Lecture Notes https://people.cs.aau.dk/~mzi/teaching/lecture-notes%20infinite%20games.pdf

data Player = Player0 | Player1 deriving (Show, Eq, Ord)

data PlayerVertex _V0 _V1 = Player0Vertex _V0 | Player1Vertex _V1 deriving (Show, Eq, Ord)

type V _V0 _V1 = Set (PlayerVertex _V0 _V1)

-- Definition 2.1 (Arena) A = (V, V_0, V_1, E)
-- Where V_0 and V_1 are disjoint subsets of V and V is the set of all vertices
-- Every vertice has at least 1 outgoing edge
data Arena _V0 _V1 = Arena
  { _V :: V _V0 _V1,
    _V0 :: Set _V0,
    _V1 :: Set _V1,
    _E :: Set (PlayerVertex _V0 _V1, PlayerVertex _V0 _V1)
  }
  deriving (Show, Eq, Ord)


-- Definition 2.3 (Play) A play in an arena is an infinite sequence of vertices rho = v_0, v_1, ... \in V^\omega
type Rho _V0 _V1 = [PlayerVertex _V0 _V1]

-- Definition 2.7 A Game G=(A, Win) where A is an arena and Win set of winning sequences
-- The Win condition could be Reach(R) or Safe(S) for Reachability or Safety games respectably

-- Definition 2.9 Winning Region W_i(G) of player i is just the set of vertices that has a winning strategy (always leads to a win)

-- 3.1 A Reachbability Game G=(A, Reach(R)) where R is a set of vertices to eventually be reached

-- Construction 3.1 Controlled Predecessor CPre_i(R)
cPre :: (Ord _V0) => (Ord _V1) => Arena _V0 _V1 -> V _V0 _V1 -> Player -> Set (PlayerVertex _V0 _V1)
cPre a r player = Map.keysSet $
      (\origin targets -> if vertexOwnedByPlayer origin player then
          -- Only need 1 edge leads to R
          any (`Set.member` r) targets
        else
          -- All edges need to lead to R
          all (`Set.member` r) targets
      ) `Map.filterWithKey` mkOutgoingEdgeMap a._E

-- Values are all the outgoing vertices
mkOutgoingEdgeMap :: (Ord _V0, Ord _V1) => Set (PlayerVertex _V0 _V1, PlayerVertex _V0 _V1) -> Map (PlayerVertex _V0 _V1) (Set (PlayerVertex _V0 _V1))
mkOutgoingEdgeMap = Set.foldr (\(v1, v2) -> Map.insertWith Set.union v1 (Set.singleton v2)) Map.empty

vertexOwnedByPlayer :: PlayerVertex _V0 _V1 -> Player -> Bool
vertexOwnedByPlayer (Player0Vertex _) Player0 = True
vertexOwnedByPlayer (Player1Vertex _) Player1 = True
vertexOwnedByPlayer _ _ = False

-- The i-attractor Attr_i(R) for R in A is just applying CPre until n
-- n >= |V| I think
-- At some point it doesn't grow so we can stop early
attr :: (Ord _V0, Show _V0, Show _V1) => (Ord _V1) => Arena _V0 _V1 -> V _V0 _V1 -> Player -> Set (PlayerVertex _V0 _V1)
attr a r player = go r
  where
    go r' =
      let r'' = r' `Set.union` cPre a r' player
       in if r'' == r' then r' else
        trace (show r' <> " " <> show r'') $
        go r''

-- The W_0(G) = Attr_0(R) the winning region for player 0 is attr A R Player0
-- W_1(G) = V / Attr_0(G) the complement of the above

-- 3.2 Safety Games G=(A, Safety(S)) where S is a set of safe verties
-- Reachablity games Player's 0 goal is to reach R
-- Safety games Player's 0 goal is to stay in S forever
-- Reachability games Player's 1 goal is to stay in V \ R forever
-- Safety games Player 1's goal is to to reach V \ S
-- Safety games are the duals of Reachability games

reachabilityGame :: (Ord _V0, Show _V0, Show _V1) => (Ord _V1) => Arena _V0 _V1 -> V _V0 _V1 -> Player -> Set (PlayerVertex _V0 _V1)
reachabilityGame a r player = let w_0 = attr a r player in
  case player of
    Player0 -> w_0
    Player1 -> a._V \\ w_0

-- Def 3.3 Dual of an Arena where we swap who owns the vertices
-- Also if the Game G=(A, Win) then the dual game is G'=(A', V^omega \ Win) where A' is the dual arena
-- Given the Arena A=(V,V_0, V_1, E), the dual Arena A', the safety game G=(A, Safety(S)), and we have a dual game G'=(A', Reach(V\S))
-- Then W_i(G) = W_{1-i}(G')
dualArena :: (Ord _V1, Ord _V0) => Arena _V0 _V1 -> Arena _V1 _V0
dualArena (Arena v v0 v1 e) = Arena (Set.map swap v) v1 v0 (Set.map (bimap swap swap) e)
  where
    swap (Player0Vertex p0V) = Player1Vertex p0V
    swap (Player1Vertex p1V) = Player0Vertex p1V

-- Exercise with Joe and Quinn
safetyGame :: (Ord _V0, Show _V0, Show _V1) => (Ord _V1) => Arena _V0 _V1 -> V _V0 _V1 -> Player -> Set (PlayerVertex _V0 _V1)
safetyGame a s player =
  let dualArena' = dualArena a
      dualPlayer = case player of
        Player0 -> Player1
        Player1 -> Player0
      r = (\case
                          Player0Vertex v -> Player1Vertex v
                          Player1Vertex v -> Player0Vertex v
                      ) `Set.map` (a._V \\ s)
      dualWinningRegion = reachabilityGame dualArena' r dualPlayer
      winningRegion = (\case
                          Player0Vertex v -> Player1Vertex v
                          Player1Vertex v -> Player0Vertex v
                      ) `Set.map` dualWinningRegion
  in
    winningRegion

-- Examples --

-- Figure 2.1
data ExamplePlayer0Vertex = V1 | V3 | V7 | V8 deriving (Show, Eq, Ord, Bounded, Enum)

data ExamplePlayer1Vertex = V0 | V2 | V4 | V5 | V6 deriving (Show, Eq, Ord, Bounded, Enum)

exampleArena :: Arena ExamplePlayer0Vertex ExamplePlayer1Vertex
exampleArena =
  Arena
    { _V = Set.map Player0Vertex (sumTypeToSet @ExamplePlayer0Vertex) `union` Set.map Player1Vertex (sumTypeToSet @ExamplePlayer1Vertex),
      _V0 = sumTypeToSet @ExamplePlayer0Vertex,
      _V1 = sumTypeToSet @ExamplePlayer1Vertex,
      _E =
        Set.fromList
          [ -- Edges from V0
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

-- Safety Game
-- Safety games are the duals of Reachability games

-- Example safety set
_S :: Set (PlayerVertex ExamplePlayer0Vertex ExamplePlayer1Vertex)
_S = fromList [Player0Vertex V1, Player1Vertex V2, Player0Vertex V3, Player1Vertex V4, Player1Vertex V5, Player0Vertex V7, Player0Vertex V8]

-- Turn a sum type into a Set of all possible values
sumTypeToSet :: (Ord a, Enum a, Bounded a) => Set a
sumTypeToSet = Set.fromList [minBound .. maxBound]

-- Construct a DotGraph from an Arena
-- Player 0 is circle
-- Player 1 is square
-- Player 0's winning regions is colored blue
-- Player 1's winning regions is colored red
-- The condition is doubly framed
arenaToDot :: (Show _V0, Show _V1, Ord _V0, Ord _V1) => Arena _V0 _V1 -> Maybe (Set (PlayerVertex _V0 _V1), Set (PlayerVertex _V0 _V1)) -> DotGraph Text
arenaToDot (Arena _V _V0 _V1 _E) regions =
  mkGraph
    (Set.toList $ Set.map (\v -> DotNode (showVertex v) (playerAttrs v <> regionAttrs v regions)) _V)
    (Set.toList $ Set.map (\(v1, v2) -> DotEdge (showVertex v1) (showVertex v2) []) _E)
  where
    showVertex (Player0Vertex v) = Text.pack $ show v
    showVertex (Player1Vertex v) = Text.pack $ show v

    playerAttrs (Player0Vertex _) = [Shape Circle]
    playerAttrs (Player1Vertex _) = [Shape BoxShape]

    regionAttrs v (Just (condition, winningRegion))
      | v `Set.member` winningRegion = [FillColor $ [WC (X11Color SteelBlue) Nothing ], Style [filled]]
      | not $ v `Set.member` winningRegion = [FillColor $ [WC (X11Color Tomato) Nothing ], Style [filled]]
      | v `Set.member` condition = [Style [SItem Dashed []]]
      | otherwise = []
    regionAttrs _ Nothing = []


addWinningRegion :: (Show _V0, Show _V1) => Set (PlayerVertex _V0 _V1) -> DotGraph Text -> DotGraph Text
addWinningRegion w g = undefined

makeDotFile :: IO ()
makeDotFile = do
  TextIO.writeFile "./arena.dot" (renderDot $ toDot $ arenaToDot exampleArena Nothing)
  putStrLn "Dot file created at arena.dot. Attempting to render with graphviz..."
  _ <- createProcess (proc "dot" ["-Tpng", "arena.dot", "-o", "arena.png"])
  putStrLn "Graphviz rendered arena.png. Attempting to open the file with default viewer..."
  _ <- createProcess (proc "xdg-open" ["./arena.png"])
  pure ()

makeReachabilityDotFile :: IO ()
makeReachabilityDotFile = do
  let
    winningRegion = reachabilityGame exampleArena _R Player0
    regions = Just (_R, winningRegion)
  TextIO.writeFile "./reachabilityGame.dot" (renderDot $ toDot $ arenaToDot exampleArena regions)
  putStrLn "Dot file created at reachabilityGame.dot. Attempting to render with graphviz..."
  _ <- createProcess (proc "dot" ["-Tpng", "reachabilityGame.dot", "-o", "reachabilityGame.png"])
  putStrLn "Graphviz rendered reachabilityGame.png. Attempting to open the file with default viewer..."
  _ <- createProcess (proc "open" ["./reachabilityGame.png"])
  pure ()

makeSafetyDotFile :: IO ()
makeSafetyDotFile = do
  let
    winningRegion = safetyGame exampleArena _S Player0
    regions = Just (_S, winningRegion)
  TextIO.writeFile "./safetyGame.dot" (renderDot $ toDot $ arenaToDot exampleArena regions)
  putStrLn "Dot file created at safetyGame.dot. Attempting to render with graphviz..."
  _ <- createProcess (proc "dot" ["-Tpng", "safetyGame.dot", "-o", "safetyGame.png"])
  putStrLn "Graphviz rendered safetyGame.png. Attempting to open the file with default viewer..."
  _ <- createProcess (proc "open" ["./safetyGame.png"])
  pure ()
