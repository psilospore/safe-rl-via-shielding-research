{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Set (Set, cartesianProduct, union, (\\), powerSet)
import qualified Data.Set as Set

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
newtype Q = Q Int deriving (Show, Eq, Ord)

-- | S: A finite state reactive system
-- _Σᵢ: Input alphabet
-- _Σᵢ = _Σᵢ¹ x _Σᵢ²
-- _Σ₀: Output alphabet
data S _Σᵢ¹ _Σᵢ² _Σ₀ _Q = S {
  -- Finite set of states
  _Q :: Set _Q
  -- Initial state
  , q₀ :: _Q
  -- Output alphabet
  , _Σ₀ :: Set _Σ₀
  -- Transition function
  , δ :: (_Q, (_Σᵢ¹, _Σᵢ²)) → _Q
  -- Complete output function
  , λ :: (_Q, (_Σᵢ¹, _Σᵢ²)) → _Σ₀
}

-- Safety automation φˢ
-- The System S satisfies the automation if the run of S only visits safe states in F
-- φˢ = (Q, q₀, Σ δ, F)
-- Σ = Σᵢ x Σₒ This is interesting because this makes the transition different?
-- Section 6: Σ₀ = Actions
data SafetyAutomaton _Σᵢ _Σₒ = SafetyAutomaton {
  _Q :: Set Qₛ -- States
  , q₀:: Qₛ
  , δ :: (Qₛ, (_Σᵢ, _Σₒ)) -> Qₛ
  , _F :: Set Qₛ -- Set of safe states where F ⊆ Q
}

-- | States for the safety automaton generated from the LTL formula
-- This is random in the paper it's qₐ, q_b, q_c, ...
newtype Qₛ = Qₛ Int deriving (Eq, Ord, Show)

-- | MDP Abstraction φᵐ
-- Which is some abstraction over the MDP
-- Σᵢ = A x L
data MDPAbstraction _Σᵢ¹ _Σᵢ² _Q = MDPAbstraction {
  _Q :: Set _Q
  , q₀ :: _Q
  , _Σᵢ :: Set (_Σᵢ¹, _Σᵢ²)
  , δ :: (_Q, (_Σᵢ¹, _Σᵢ²)) -> _Q
  , _F :: Set _Q
}


-- | Convert an LTL formula to a Safety Automaton
-- TODO this is LLM generated might be incorrect
ltlToAutomaton :: forall _APᵢ _AP₀ _Σᵢ _Σₒ. LTL _APᵢ _AP₀ -> SafetyAutomaton _Σᵢ _Σₒ
ltlToAutomaton ltlFormula = SafetyAutomaton {
    _Q = states,                  -- The set of all possible states (from LTL subformulas) this is random in the paper it's qₐ, q_b, q_c, ...
    q₀ = initialState,             -- Initial state based on the LTL formula
    δ = transitionFunction,        -- Transition function based on Σᵢ × Σₒ
    _F = safeStates                -- Set of safe states
  }
  where
    -- Step 1: Define states based on LTL subformulas
    states :: Set Qₛ
    states = undefined -- TODO

    -- Step 2: Define the initial state based on the formula
    initialState :: Qₛ
    initialState = undefined -- getInitialState ltlFormula

    -- Step 3: Transition function (maps a state and input to a new state)
    transitionFunction :: (Qₛ, (_Σᵢ, _Σₒ)) -> Qₛ
    transitionFunction (q, (_Σᵢ, _Σₒ)) = undefined
      -- determineNextState q (_Σᵢ, _Σₒ)

    -- Step 4: Define safe states
    -- Not sure if this is correct
    safeStates :: Set Qₛ
    safeStates = undefined -- filterSafeStates states ltlFormula

-- | 2 player Safety Game G
-- Section 6 describes tuple
data Game _G _Σᵢ _Σₒ = Game {
    _G :: Set _G -- Finite set of game states
    , q₀ :: _G -- Initial state
    , _Σᵢ :: Set _Σᵢ -- Input alphabet
    , _Σₒ :: Set _Σₒ -- Output alphabet
    , δ :: (_G, (_Σᵢ, _Σₒ)) -> _G -- Transition function
    , _Fᵍ :: Set _G -- Accepting states
}

type Σ = Set Int

-- L but maybe just for preemptive shields
-- e.g.  {level < 1, 1 ≤ level ≤ 99, level > 99}
-- type Σᵢ¹ = L


-- | Propositions TODO not sure how to represent this
type Prop = String

-- | LTL Formula data type
-- AP = APᵢ union AP₀ the set of atomic propositions
data LTL _APᵢ _AP₀
  = APᵢ _APᵢ -- TODO maybe merge these first two?
  | AP₀ _AP₀
  | Not (LTL _APᵢ _AP₀)
  | And (LTL _APᵢ _AP₀) (LTL _APᵢ _AP₀)
  | Or (LTL _APᵢ _AP₀) (LTL _APᵢ _AP₀)
  | Implies (LTL _APᵢ _AP₀) (LTL _APᵢ _AP₀)
  | X (LTL _APᵢ _AP₀)
  | G (LTL _APᵢ _AP₀)
  | F (LTL _APᵢ _AP₀)
  | U (LTL _APᵢ _AP₀) (LTL _APᵢ _AP₀)
  deriving (Eq, Show)

-- | Sugar
(&&&) = And
(|||) = Or
(-->) = Implies

-- Example in paper
data LTLExampleAP = ExR | ExG deriving (Show, Eq, Ord)

-- Not sure which are AP₀ and APᵢ in this context
exampleLtlFormula :: LTL () LTLExampleAP
exampleLtlFormula = G ( AP₀ ExR ||| X (AP₀ ExG))

-- | Section 6 a shield is computed from an abstraction of the MDP φᵐ and the safety automaton φˢ
-- φˢ has Σ = Σᵢ x Σₒ and Σₒ = action
computePreemptiveShield :: forall action label _Qₘ. (Ord action, Ord label, Ord _Qₘ) => SafetyAutomaton label action -> MDPAbstraction action label _Qₘ -> S label action (Set action) (Qₛ, _Qₘ)
computePreemptiveShield φˢ φᵐ =
  -- 1. Translate φˢ and φᵐ into a safety game
  -- The MDP abstraction's Σᵢ = A x L therefore:
  let _A :: Set action
      _A = Set.map fst φᵐ._Σᵢ -- Actions
      _L :: Set label
      _L = Set.map snd  φᵐ._Σᵢ -- Labels
      _G :: Set (Qₛ, _Qₘ) -- A product of both automata's states
      _G = Set.cartesianProduct φˢ._Q φᵐ._Q
      _G' :: Game (Qₛ, _Qₘ) label action
      _G' = Game {
          _G = φˢ._Q `cartesianProduct` φᵐ._Q
          , q₀ = (φˢ.q₀, φᵐ.q₀)
          , _Σᵢ = _L
          ,  _Σₒ = _A
          , δ = \((q, qₘ), (l , a)) -> (φˢ.δ (q, (l, a)), φᵐ.δ (qₘ, (a, l)))
                                                                                                -- ^^  paper says this is (l, a) maybe they have that flipped? or I am wrong
          , _Fᵍ = (φˢ._F `cartesianProduct` φᵐ._Q) `union` (φˢ._Q `cartesianProduct` (φᵐ._F \\ φᵐ._F))
      }
      -- 2. Compute the winning strategy TODO this is described in Shield Synthesis but I think we can use SMT for this part
      _W :: Set (Qₛ, _Qₘ)
      _W = undefined
      -- Maybe this? not sure
      _W' :: Set Qₛ
      _W' = Set.map fst _W
      -- 3. For pre-emptive shielding translate G and W into a reactive system
      -- Powerset of actions (the shield outputs a set of safe actions rather than a single action)
      _2ᴬ :: Set (Set action)
      _2ᴬ = powerSet _A
      _S :: S label action (Set action) (Qₛ, _Qₘ)
      _S = S {
        _Q = _G
        , q₀ = _G'.q₀
        -- A x L which we can get from the MDP abstraction
        --, _Σᵢ = φᵐ._Σᵢ should just be type param
        -- The output is a set of actions 2ᴬ
        , _Σ₀ = _2ᴬ
        , δ = \(g, (l, a)) -> _G'.δ (g, (l, a))
        -- TODO hmm notation seems to omit
       -- safety automation returns  , δ :: (Qₛ, (labelᵢ, action)) -> Qₛ
        -- W is a (Q_S, _Q_m)
        , λ = \((g, (l, _)):: ((Qₛ, _Qₘ), (label, action))) -> Set.filter (\a -> φˢ.δ (fst g, (l, a)) `elem` _W') _A
        -- They also don't take the fst of g
      }
  in _S

-- Optionally remove states that are not reachable from the initial state
-- prune :: S -> S
-- prune s = s -- TODO optional just going to return the input for now

-- Label set e.g. {level < 1, 1 ≤ level ≤ 99, level > 99}
type L = Set Prop

-- Examples --

-- | Water Tank Example
-- watertankL :: L
-- watertankL = Set.fromList ["level < 1", "1 ≤ level ≤ 99", "level > 99"]


-- Actions are just {open, close}
data WatertankA = OpenAction | CloseAction deriving (Show, Eq, Ord)
data WatertankL = LevelLessThan99 | LevelBetween1And99 | LevelGreaterThan1 deriving (Show, Eq, Ord)

-- AP seems to equal Action | Labels
-- I think we don't need this now we reformulated LTL to use APᵢ and AP₀
-- data WatertankAP = OpenAP | CloseAP | LevelLessThan100AP | LevelGreaterThan0AP deriving (Show, Eq, Ord)

watertankφ :: LTL WatertankL WatertankA
watertankφ = G (APᵢ LevelGreaterThan1) &&& G (APᵢ LevelLessThan99)
   -- TODO &&& G ((AP "open" &&& X (AP "close")) --> (X X (AP "close") &&& XXX (AP "close")))

-- Output from Safety Automaton is an action
watertankφˢ :: SafetyAutomaton WatertankL WatertankA
watertankφˢ = ltlToAutomaton watertankφ

-- | TODO
data WatertankQ = WatertankQ0 | WatertankQ1 | WatertankQ2 deriving (Show, Eq, Ord)

-- The MDP abstraction has Σᵢ = A x L
watertankφᵐ :: MDPAbstraction WatertankA WatertankL WatertankQ
watertankφᵐ = undefined -- TODO from the paper

watertankPreemptiveShield :: S WatertankL WatertankA (Set WatertankA) (Qₛ, WatertankQ)
watertankPreemptiveShield = computePreemptiveShield watertankφˢ watertankφᵐ


-- | Arbiter example from the shortened paper
-- This seems to be a request based system where you can request A or B

data ArbiterΣᵢ = RequestARequestB | DenyARequestB | RequestADenyB | DenyADenyB deriving (Show, Eq, Ord)

data ArbiterΣₒ = GrantedAGrantedB | DeniedAGrantedB | GrantedADeniedB | DeniedADeniedB deriving (Show, Eq, Ord)

data ArbiterQ = QIdle | QGrantedA | QGrantedB deriving (Show, Eq, Ord)

-- Turn a sum type into a Set of all possible values
sumTypeToSet :: (Ord a, Enum a, Bounded a) => Set a
sumTypeToSet = Set.fromList [minBound..maxBound]
