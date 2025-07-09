{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module LtlToAutomaton where

import ShieldingV2
import HaskellHOAFParser
import Data.Text (Text, unpack)
import System.Process (readProcess)

-- Convert LTL formula to a String that `owl` can process
ltlToString :: LTL Text Text -> String
ltlToString (APᵢ ap) = unpack ap
ltlToString (AP₀ ap) = unpack ap
ltlToString (Not ltl) = "!" ++ ltlToString ltl
ltlToString (And l1 l2) = "(" ++ ltlToString l1 ++ " & " ++ ltlToString l2 ++ ")"
ltlToString (Or l1 l2) = "(" ++ ltlToString l1 ++ " | " ++ ltlToString l2 ++ ")"
ltlToString (Implies l1 l2) = "(" ++ ltlToString l1 ++ " -> " ++ ltlToString l2 ++ ")"
ltlToString (X ltl) = "X " ++ ltlToString ltl
ltlToString (G ltl) = "G " ++ ltlToString ltl
ltlToString (F ltl) = "F " ++ ltlToString ltl
ltlToString (U l1 l2) = "(" ++ ltlToString l1 ++ " U " ++ ltlToString l2 ++ ")"

ltlRequest :: LTL Text Text -> IO String
ltlRequest ltlFormula = do
  let ltlString = ltlToString ltlFormula
  -- TODO maybe a different mode?
  readProcess "./owl" ["ltl2nba"] ltlString

-- | Generate a safety automaton from an LTL formula.
ltlToAutomaton :: forall _APᵢ _AP₀ _Σᵢ _Σₒ. LTL _APᵢ _AP₀ -> IO (SafetyAutomaton _Σᵢ _Σₒ)
ltlToAutomaton ltlFormula = do
  requestLtl <- ltlRequest ltlFormula
  responseHoaf :: HOA <- parseHOAF <$> ltlRequest ltlFormula
  return $ hoaToSafetyAutomaton @_Σᵢ @_Σₒ responseHoaf
  where
    ltlRequest = undefined
    parseHOAF = undefined

-- Convert HOA to SafetyAutomaton
-- TODO this is probably wrong?
hoaToSafetyAutomaton :: forall _Σᵢ _Σₒ. (Enum _Σᵢ, Enum _Σₒ) =>  HOA -> SafetyAutomaton _Σᵢ _Σₒ
hoaToSafetyAutomaton hoa = SafetyAutomaton
  { _Q = statesSet
  , q₀ = initialState
  , δ = transitionFunction
  , _F = safeStates
  }
  where
    statesSet = foldMap (pure . stateId) (states hoa)
    initialState = head (startStates hoa)
    safeStates = foldMap (pure . stateId) $ filter hasSafeTransitions (states hoa)
    transitionFunction (q, (input, output)) = do
      state <- lookup q (map (\s -> (stateId s, transitions s)) (states hoa))
      target state input output
    hasSafeTransitions state =
      any (\t -> accSet t /= Nothing) (transitions state)
    target state _ _ = Nothing -- TODO
