{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Data.Set (Set, cartesianProduct, union)

-- TODOs
-- Maybe start more top down instead so look at later pages and try to implement backwards.
-- I did the other approach and it was a bit harder to follow.
-- Maybe starting at preemptive shield which uses MDPs which then appl6y a shield to the actions (removing unsafe actions might make more sense)
-- OR do safety games but that's used in the above. If I struggle with the above I'll try this.
-- Section 6 contains main stuff I think I understand mostly everything up until then


-- | Credits to TODO
-- Just guessing the types or stubbing them

-- | Unicode characters
-- You can setup latex input mode in emacs to use these
-- φ \varphi Specification
-- φˢ \varphi^s Safety automation
-- φᵐ \varphi^m MDP abstraction
-- Σ \Sigma Alphabet sometimes composed of the input and output alphabet Σ = Σᵢ x Σₒ
-- Σᵢ \Sigma_i Input alphabet
-- Σₒ \Sigma_o Output alphabet
-- δ \delta Transition function
-- ρ \rho
-- λ \lambda Output function
-- ω \omega
-- Gω G\omega Really G with a super script ω (omega)
-- Many of these are described in section 3 Preliminaries

-- Automata States
data Q = Q Int deriving (Show, Eq, Ord)

-- | S: A finite state reactive system
-- _Σᵢ: Input alphabet
-- _Σᵢ = _Σᵢ¹ x _Σᵢ²
-- _Σ₀: Output alphabet
data S _Σᵢ¹ _Σᵢ² _Σ₀ = S {
  -- Finite set of states
  _Q :: Set Q
  -- Initial state
  , q₀ :: Q
  -- Input alphabet
  , _Σᵢ :: (_Σᵢ¹, _Σᵢ²)
  -- Output alphabet
  , _Σ₀ :: _Σ₀
  -- Transition function
  , δ :: (Q, (_Σᵢ¹, _Σᵢ²)) → Q
  -- Complete output function
  , λ :: (Q, (_Σᵢ¹, _Σᵢ²)) → _Σ₀
}

-- Safety automation φˢ
-- The System S satisfies the automation if the run of S only visits safe states in F
-- Σᵢ = Σᵢ¹ x Σᵢ² but can also be decomposed to TODO
data SafetyAutomaton _Σᵢ¹ _Σᵢ² = SafetyAutomaton {
  _Q :: Set Q -- States
  , q₀:: Q
  , _Σ :: Set (_Σᵢ¹, _Σᵢ²) -- Input alphabet
  , δ :: (Q, (_Σᵢ¹, _Σᵢ²)) -> Q
  , _F :: Set Q -- Set of safe states where F ⊆ Q
  }

-- | MDP Abstraction φᵐ
-- Which is some abstraction over the MDP
-- Σᵢ = A x L
data MDPAbstraction _Σᵢ¹ _Σᵢ² = MDPAbstraction {
  _Q :: Set Q
  , q₀ :: Q
  , _Σᵢ :: Set (_Σᵢ¹, _Σᵢ²)
  , δ :: (Q, (_Σᵢ¹, _Σᵢ²)) -> Q
  , _F :: Set Q
}

-- | LTL to Safety Automaton
-- TODO
ltlToAutomaton :: LTL -> SafetyAutomaton
ltlToAutomaton = undefined

-- | 2 player Game
data Game _G _Σᵢ _Σₒ = Game {
    _G :: Set _G -- Finite set of game states
    , g₀ :: _G -- Initial state
    , _Σᵢ :: _Σᵢ -- Input alphabet
    , _Σₒ :: _Σₒ -- Output alphabet
    , δ :: (_G, _Σᵢ, _Σₒ) -> _G -- Transition function
    , win :: _G -- The winning region
}

type Σ = Set Int

-- L but maybe just for preemptive shields
-- e.g.  {level < 1, 1 ≤ level ≤ 99, level > 99}
-- type Σᵢ¹ = L


-- | Propositions TODO not sure how to represent this
type Prop = String

-- | LTL Formula data type
-- ap: Set of Atomic Propositions
data LTL ap
  = AP ap            -- Atomic proposition
  | Not LTL
  | And LTL LTL
  | Or LTL LTL
  | Implies LTL LTL
  | X LTL            -- Next
  | G LTL            -- Globally/Always
  | F LTL            -- Eventually
  | U LTL LTL        -- Until
  deriving (Eq, Show)

-- | Sugar
(&&&) = And
(|||) = Or
(-->) = Implies

-- Example in paper
data LTLExampleAP = ExR | ExG deriving (Show, Eq, Ord)
exampleLtlFormula :: LTL LTLExampleAP
exampleLtlFormula = G ( AP ExR ||| X (AP ExG))


-- | A trace I think?
type Trace = [Set Prop]

-- | Does the trace satisfy the LTL
-- TODO LLM generated might be wrong. This tracks the index but I think we can write this recursively?
satisfies :: Trace -> Int -> LTL -> Bool
satisfies σ idx formula = case formula of
  AP p ->
    idx < length σ && p `elem` (σ !! idx)
  Not f ->
    not (satisfies σ idx f)
  And f1 f2 ->
    satisfies σ idx f1 && satisfies σ idx f2
  Or f1 f2 ->
    satisfies σ idx f1 || satisfies σ idx f2
  Implies f1 f2 ->
    if satisfies σ idx f1 then satisfies σ idx f2 else True
  X f ->
    satisfies σ (idx + 1) f
  G f ->
    all (\i -> satisfies σ i f) [idx .. length σ - 1]
  F f ->
    any (\i -> satisfies σ i f) [idx .. length σ - 1]
  U f1 f2 ->
    let future = drop idx σ
        holdsAt i = satisfies σ i f2 && all (\j -> satisfies σ j f1) [idx .. i - 1]
    in any holdsAt [idx .. length σ - 1]


-- | Section 6 a shield is computed from an abstraction of the MDP φᵐ and the safety automaton φˢ
computePreemptiveShield :: SafetyAutomaton -> MDPAbstraction -> S
computePreemptiveShield φˢ φᵐ =
  -- 1. Translate φˢ and φᵐ into a safety game
  -- The MDP abstraction's Σᵢ = A x L therefore:
  let _A = fst <$> φᵐ._Σᵢ -- Actions
      _L = snd <$> φᵐ._Σᵢ -- Labels
      _G = Set.cartesianProduct φˢ._Q φᵐ._Q -- A product of both automata's states
      _G' = SafetyGame {
            _G = φˢ._Q `cartesianProduct` φᵐ._Q
            , q₀ = (φˢ.q₀, φᵐ.q₀)
            , _Σᵢ = _L
           ,  _Σₒ = _A
           -- Paper might be missing a qₘ I think
           -- I wonder if I should use tuples instead of functions like in python
           -- Not sure what is better when I start using an SMT solver
           , δ = \(q, qₘ) l a -> (φˢ.δ q (l, a), φᵐ.δ qₘ, (l, a))
           , _Fᵍ = (φˢ._F `cartesianProduct` φᵐ.Q) `union` (φˢ.Q `cartesianProduct` (φᵐ._F \\ φᵐ._F))
          }
      -- 2. Compute the winning strategy TODO this is described in Shield Synthesis but I think we can use SMT for this part
      _W = undefined
      -- 3. For pre-emptive shielding translate G and W into a reactive system
      _S = Shield {
        _Q = _G
        , q₀ = _G'.q₀
        -- A x L which we can get from the MDP abstraction
        , _Σᵢ = φᵐ._Σᵢ
        -- The output is a set of actions 2ᴬ
        , _Σₒ = powerset _A
        , δ = \(g, l, a) -> _G'.δ (g, l, a)
        , λ = \(g, l) -> Set.filter (\a -> φˢ.δ (g, l, a) `elem` _W) _A
      }
  in prune _S

-- Optionally remove states that are not reachable from the initial state
prune :: S -> S
prune s = s -- TODO optional just going to return the input for now

-- Label set e.g. {level < 1, 1 ≤ level ≤ 99, level > 99}
type L = Set Prop

-- Examples --

-- | Water Tank Example
-- watertankL :: L
-- watertankL = Set.fromList ["level < 1", "1 ≤ level ≤ 99", "level > 99"]

data WatertankAP = OpenAP | CloseAP | LevelLessThan100AP | LevelGreaterThan0AP deriving (Show, Eq, Ord)

-- Actions are just {open, close}
newtype WatertankA = Open | Close deriving (Show, Eq, Ord)

data WatertankL = LevelLessThan1 | LevelBetween1And99 | LevelGreaterThan99 deriving (Show, Eq, Ord)

watertankφ :: LTL WatertankAP
watertankφ = G (AP LevelGreaterThan0) &&& G (AP LevelLessThan100)
   -- TODO &&& G ((AP "open" &&& X (AP "close")) --> (X X (AP "close") &&& XXX (AP "close")))

watertankφˢ :: SafetyAutomaton WatertankAP 
watertankφˢ = ltlToAutomaton watertankφ

watertankφᵐ :: MDPAbstraction WatertankA WatertankL
watertankφᵐ = undefined -- TODO from the paper

watertankPreemptiveShield :: S
watertankPreemptiveShield = computePreemptiveShield watertankφˢ watertankφᵐ


-- | Arbiter example from the shortened paper
-- This seems to be a request based system where you can request A or B

data ArbiterΣᵢ = RequestARequestB | DenyARequestB | RequestADenyB | DenyADenyB deriving (Show, Eq, Ord)

data ArbiterΣₒ = GrantedAGrantedB | DeniedAGrantedB | GrantedADeniedB | DeniedADeniedB deriving (Show, Eq, Ord)

data ArbiterQ = QIdle | QGrantedA | QGrantedB deriving (Show, Eq, Ord)

