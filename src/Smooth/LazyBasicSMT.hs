module Smooth.LazyBasicSMT(
  lazyBasicSMT) where

import Data.List as L
import Data.Map as M
import Data.Set as S

import FirstOrderTheory.Syntax as Fol
import FirstOrderTheory.Theory
import Proper.Clause as Prop
import Proper.CNF

-- |A simple, lazy SMT solver based on Algorithm 11.2.1
-- on page 245 of 'Decision Procedures: An Algorithmic Point of View'
lazyBasicSMT :: (FirstOrderTheory t) =>
                t ->
                NNFFormula ->
                Bool
lazyBasicSMT theory formula = trySAT theory cnfFormula
  where
    cnfFormula = toPropositionalCNF formula

trySAT :: (FirstOrderTheory t) => t -> CNF Literal -> Bool
trySAT theory cnfForm = case formIsSat of
  True -> case assignmentIsSatInTheory of
    True -> True
    False -> trySAT theory (addClause propLemma cnfForm)
  False -> False
  where
    (formIsSat, satisfyingAsg) = case naiveSAT cnfForm of
      Just asg -> (True, asg)
      _ -> (False, M.empty)
    literals = buildClauseFromSatAsg satisfyingAsg
    (assignmentIsSatInTheory, lemma) = decideSat theory literals
    propLemma = clause $ L.map propNegates $ S.toList lemma

propNegates :: Literal -> Prop.Atom Literal
propNegates l = case isNeg l of
  True -> negation $ Prop.lit $ Fol.lit $ Fol.getAtom l
  False -> Prop.lit l

buildClauseFromSatAsg :: Map Literal Bool -> Set Literal
buildClauseFromSatAsg lits = S.fromList $ L.map negateFalseLit $ M.toList lits

negateFalseLit :: (Literal, Bool) -> Literal
negateFalseLit (l, True) = l
negateFalseLit (l, False) = Fol.negateLit l
