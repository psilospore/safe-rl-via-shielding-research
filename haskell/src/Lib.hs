{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Set (Set, cartesianProduct, union)
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
  -- Output alphabet
  , _Σ₀ :: _Σ₀
  -- Transition function
  , δ :: (Q, (_Σᵢ¹, _Σᵢ²)) → Q
  -- Complete output function
  , λ :: (Q, (_Σᵢ¹, _Σᵢ²)) → _Σ₀
}

-- Safety automation φˢ
-- The System S satisfies the automation if the run of S only visits safe states in F
-- φˢ = (Q, q₀, Σ δ, F)
-- Σ = Σᵢ x Σₒ This is interesting because this makes the transition different?
-- Section 6: Σ₀ = Actions
data SafetyAutomaton _Σᵢ _Σₒ = SafetyAutomaton {
  _Q :: Set Q -- States
  , q₀:: Q
  , δ :: (Q, (_Σᵢ, _Σₒ)) -> Q
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
-- TODO should pass ap to Safety Automaton
-- But it seems that the Σᵢ¹ is {open, close} and Σᵢ² is {level < 1, 1 ≤ level ≤ 99, level > 99}
-- Which is differnt AP from the
ltlToAutomaton :: forall _APᵢ _AP₀. LTL _APᵢ _AP₀ -> SafetyAutomaton _APᵢ _AP₀
ltlToAutomaton = undefined

-- | 2 player Safety Game G
-- Section 6 describes tuple
data Game _G _Σᵢ _Σₒ = Game {
    _G :: Set _G -- Finite set of game states
    , q₀ :: _G -- Initial state
    , _Σᵢ :: _Σᵢ -- Input alphabet
    , _Σₒ :: _Σₒ -- Output alphabet
    , δ :: (_G, _Σᵢ, _Σₒ) -> _G -- Transition function
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
computePreemptiveShield :: forall _A _L. SafetyAutomaton _A _L -> MDPAbstraction _A _L -> S _L _A _A
computePreemptiveShield φˢ φᵐ =
  -- 1. Translate φˢ and φᵐ into a safety game
  -- The MDP abstraction's Σᵢ = A x L therefore:
  let _A = fst <$> φᵐ._Σᵢ -- Actions
      _L = snd <$> φᵐ._Σᵢ -- Labels
      _G = Set.cartesianProduct φˢ._Q φᵐ._Q -- A product of both automata's states
      _G' = Game {
            _G = φˢ._Q `cartesianProduct` φᵐ._Q
            , q₀ = (φˢ.q₀, φᵐ.q₀)
            , _Σᵢ = _L
           ,  _Σₒ = _A
           -- Paper might be missing a qₘ I think
           -- I wonder if I should use tuples instead of functions like in python
           -- Not sure what is better when I start using an SMT solver
           , δ = \(q, qₘ) l a -> (φˢ.δ q (l, a), φᵐ.δ qₘ, (l, a))
           , _Fᵍ = (φˢ._F `cartesianProduct` φᵐ._Q) `union` (φˢ._Q `cartesianProduct` (φᵐ._F \\ φᵐ._F))
          }
      -- 2. Compute the winning strategy TODO this is described in Shield Synthesis but I think we can use SMT for this part
      _W = undefined
      -- 3. For pre-emptive shielding translate G and W into a reactive system
      _S = S {
        _Q = _G
        , q₀ = _G'.q₀
        -- A x L which we can get from the MDP abstraction
        --, _Σᵢ = φᵐ._Σᵢ should just be type param
        -- The output is a set of actions 2ᴬ
        , _Σ₀ = powerset _A
        , δ = \(g, l, a) -> _G'.δ (g, l, a)
        , λ = \(g, l) -> Set.filter (\a -> φˢ.δ (g, l, a) `elem` _W) _A
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
data WatertankL = LevelLessThan1 | LevelBetween1And99 | LevelGreaterThan99 deriving (Show, Eq, Ord)

-- AP seems to equal Action | Labels
-- I think we don't need this now we reformulated LTL to use APᵢ and AP₀
-- data WatertankAP = OpenAP | CloseAP | LevelLessThan100AP | LevelGreaterThan0AP deriving (Show, Eq, Ord)

watertankφ :: LTL WatertankA WatertankL
watertankφ = G (AP LevelGreaterThan0) &&& G (AP LevelLessThan100)
   -- TODO &&& G ((AP "open" &&& X (AP "close")) --> (X X (AP "close") &&& XXX (AP "close")))

watertankφˢ :: SafetyAutomaton WatertankA WatertankL
watertankφˢ = ltlToAutomaton watertankφ

watertankφᵐ :: MDPAbstraction WatertankA WatertankL
watertankφᵐ = undefined -- TODO from the paper

watertankPreemptiveShield :: S WatertankA WatertankL WatertankA
watertankPreemptiveShield = computePreemptiveShield watertankφˢ watertankφᵐ


-- | Arbiter example from the shortened paper
-- This seems to be a request based system where you can request A or B

data ArbiterΣᵢ = RequestARequestB | DenyARequestB | RequestADenyB | DenyADenyB deriving (Show, Eq, Ord)

data ArbiterΣₒ = GrantedAGrantedB | DeniedAGrantedB | GrantedADeniedB | DeniedADeniedB deriving (Show, Eq, Ord)

data ArbiterQ = QIdle | QGrantedA | QGrantedB deriving (Show, Eq, Ord)

