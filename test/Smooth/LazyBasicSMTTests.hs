module Smooth.LazyBasicSMTTests(
  allLazyBasicSMTTests) where

import DifferenceLogic.Solver
import FirstOrderTheory.Syntax
import Smooth.LazyBasicSMT
import Smooth.TestUtils

allLazyBasicSMTTests = do
  testFunction (lazyBasicSMT integerDifferenceLogic) idfCases

idfCases =
  [(nnfLit (lit $ atom "<" [func "-" [var "a", var "b"], intConst 12]), True)]
    
