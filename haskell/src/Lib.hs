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

-- | MDP States
data S = S Int

-- | MDP Booleans
data A = A Bool

-- | Probablistic Transition Function
type P = (S, A) -> Set (S, Float)

-- | Reward funtion
type R = (S, A, S) -> Float

-- | Markov Decision Process
-- I wonder if we even need this given they start talking about an abstraction over the MDP φ
data M = M {
  _S :: Set S
  , sᵢ :: S -- Initial state
  , _A :: Set A -- Finite set of boolean actions
  , _P :: P
  , _R :: R
}

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
  , _Σᵢ :: _Σᵢ
  -- Output alphabet
  , _Σ₀ :: _Σ₀
  -- Transition function
  , δ :: (Q, _Σᵢ) → _Σᵢ
  -- Complete output function
  , λ :: (Q, _Σᵢ) → _Σ₀
}

-- Safety automation φˢ
-- The System S satisfies the automation if the run of S only visits safe states in F
data SafetyAutomation _Σᵢ¹ _Σᵢ² = SafetyAutomation {
  _Q :: Set Q -- States
  , q₀:: Q
  , _Σ :: _Σ
  , δ :: (Q, (_Σᵢ¹, _Σᵢ²)) -> Q
  , _F :: Set Q -- Set of safe states where F ⊆ Q
  }

-- | MDP Abstraction φᵐ
-- Which is some abstraction over the MDP
-- The confusing part is that they state the abstraction is also a Safety word automation so like SafetyAutomation φˢ?
-- Maybe that's fine it's just an automata but can we assume we know the safe states F? Perhaps I guess?
data MDPAbstraction = MDPAbstraction {
  _Q :: Set Q
  , q₀ :: Q
  , _Σ :: _Σ
  , δ :: (Q, _Σ) -> Q
  , _F :: Set Q
}


-- | 2 player Game
data Game _G = Game {
    _G :: Set _G -- Finite set of game states
    , g₀ :: _G -- Initial state
    , _Σᵢ :: _Σᵢ -- Input alphabet
    , _Σₒ :: _Σₒ -- Output alphabet
    , δ :: (_G, Σᵢ, Σₒ) -> _G -- Transition function
    , win :: _Gω
}

-- | Used for synthesizing the shield
-- Defines a set Fᵍ  ⊆ G of safe states
-- Where win(g₀, g₁, ...) iff ∀i ≥ 0. gᵢ ∈ Fᵍ
-- I think we can solve for this using SMT. In the original implementation they used BDDs.
-- safetyGame ∷ G -> G
-- safetyGame g = undefined


safetyGame ∷ S _Σᵢ¹ _Σᵢ² _Σ₀ -> SafetyAutomation -> S _ _
safetyGame=
    let g = undefined
        q = undefined
        _Σᵢ = undefined
        _Σₒ= undefined
        δ' = undefined -- (g, σᵢ ) = (g, σᵢ, ρ(g, σᵢ))
        ρ = undefined
    in undefined -- TODO S q _Σᵢ _Σₒ δ'

-- | Uses the safety game to synthesize a shield which implements the winning strategy in a new reactive system (Is it a finite reactive system?)
-- shield  ∷ Shield _Σᵢ _Σ₀
-- shield =
--     let g = undefined
--         q = undefined
--         _Σᵢ = undefined
--         _Σₒ= undefined
--         δ' = undefined -- (g, σᵢ ) = (g, σᵢ, ρ(g, σᵢ))
--         ρ = undefined
--     in undefined -- TODO S q _Σᵢ _Σₒ δ'

type Σ = Set Int

-- L but maybe just for preemptive shields
-- e.g.  {level < 1, 1 ≤ level ≤ 99, level > 99}
-- type Σᵢ¹ = L

-- A but maybe just for preemtive shields
-- e.g. {Open, Close}
type Σᵢ² = A

-- | Propositions TODO not sure how to represent this
type Prop = String

-- | LTL Formula data type
data LTL
  = AP Prop          -- Atomic proposition
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
exampleLtlFormula :: LTL
exampleLtlFormula = G ( AP "r" ||| X (AP "g"))

waterTankExampleLtlFormula = G (AP "level > 0") &&& G (AP "level < 100")
   -- TOOD &&& G ((AP "open" &&& X (AP "close")) --> (X X (AP "close") &&& XXX (AP "close")))

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
    in any (\i -> holdsAt i) [idx .. length σ - 1]

-- | Preemptive Shield iteration
-- Given the time step compute a set of safe actions (remove unsafe actions)
-- Environment executes one action and moves to the next state providing a reward
-- I think the paper describes this more clearly even though this might be harder to use
-- The input alphabet Σᵢ = Σᵢ¹ x Σᵢ² = L x A
-- The output alphabet Σₒ = 2ᴬ
-- Where A is the boolean 'Actions' in the MDP
-- preemptiveShieldIter :: (L, A) -> Set A
-- preemptiveShieldIter (l, a) = undefined

-- This is actually properties of the shield maybe not what we want
-- But overall it can also be seem as transforming a MDP into a new MDP (section 5.1)
preemptiveShield :: M -> Shield -> M
preemptiveShield _M _S =
  -- Product of the original MDP and the state space of the shield
  let _S' = undefined
      -- For each s' in S' create a new subset of available actions
      -- Apply the shield to Aₛ to get A'ₛ
      -- So that gets us a bunch of A'ₛ but how do we get A'?
      _A' = undefined
      -- We only want transition functions from P for actions in A'ₛ
      _P' = undefined
      _R' = undefined
  in M _S' M.s_i  _A' _P' _R'

-- | Section 6 a shield is computed from an abstraction of the MDP φᵐ and the safety automaton φˢ
-- 1. Translate φˢ and φᵐ into a safety game

computeShield :: SafetyGame -> MDPAbstraction -> S
computeShield φˢ φᵐ =
  -- 1. Translate φˢ and φᵐ into a safety game
  -- TODO this is similar to shieldpy

  -- The MDP abstraction's Σᵢ = A x L
  let _A = fst <$> φᵐ._Σᵢ -- Actions
      _L = snd <$> φᵐ._Σᵢ -- Labels
      _G = Set.cartesianProduct φˢ._Q φᵐ._Q -- A product of both automata's states
      _G' = SafetyGame {
            _G = φˢ._Q `cartesianProduct` φᵐ._Q
            , q₀ = (φˢ.q₀, φᵐ.q₀)
            , Σᵢ = _L
           ,  Σₒ = _A
           -- Paper might be missing a qₘ I think
           -- I wonder if I should use tuples instead of functions like in python
           -- Not sure what is better when I start using an SMT solver
           , δ = ((q, qₘ), l :: L, a :: A) -> (φˢ.δ q (l, a), φᵐ.δ qₘ, (l, a))
           , Fᵍ = (φˢ._F `cartesianProduct` φᵐ.Q) `union` (φˢ.Q `cartesianProduct` (φᵐ._F \\ φᵐ._F))
          }
-- 2. Compute the winning strategy TODO this is described in Shield Synthesis but I think we can use SMT for this part
    _W = undefined
-- 3. For pre-emptive shielding translate G and W into a reactive system
    in Shield {
      _Q = _G
      , q₀ = _G'.q₀
      , _Σᵢ = φᵐ._Σᵢ -- A x L which we can get from the MDP abstraction
      -- 2ᴬ so I think it's a powerset? Might make sense because it's going to suggest a set of safe actions
      -- Or it's a mapping of Action to {0, 1} where 0 is unsafe and 1 is safe?
      , _Σₒ = powerset _A
      , δ = (g, l, a) -> _G'.δ (g, l, a)
      , λ = (g, l, a) -> if g ∈ _W  -- TODO yeah actually make it  a tuple or Maybe
    }


-- Optionally remove states that are not reachable from the initial state
prune :: S -> S
prune s = s -- TODO



-- Label set e.g. {level < 1, 1 ≤ level ≤ 99, level > 99}
type L = Set Prop


-- | Water Tank Example
watertankL :: L
watertankL = Set.fromList ["level < 1", "1 ≤ level ≤ 99", "level > 99"]

-- waterTankA :: A
-- waterTankA = Set.fromList [ "opened", "closed"]


-- | Abiter example from the shortened paper
-- This seems to be a request based system where you can request A or B

data ArbiterΣᵢ = RequestARequestB | DenyARequestB | RequestADenyB | DenyADenyB deriving (Show, Eq, Ord)

data ArbiterΣₒ = GrantedAGrantedB | DeniedAGrantedB | GrantedADeniedB | DeniedADeniedB deriving (Show, Eq, Ord)

data ArbiterQ = QIdle | QGrantedA | Q GrantedB deriving (Show, Eq, Ord)
