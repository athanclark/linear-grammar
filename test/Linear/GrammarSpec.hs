module Linear.GrammarSpec (main, spec) where

import Linear.Grammar

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LinAst" $ do
    it "`multLin` should be idempotent" $
      property prop_multReduction_Idempotency
    it "`addLin` should not add or remove variables" $
      property prop_addMutation_NonForgetful
  describe "LinExpr" $ do
    it "`removeDupLin` should be idempotent" $
      property prop_removeDup_Idempotency

prop_multReduction_Idempotency :: LinAst -> Bool
prop_multReduction_Idempotency x = multLin x == multLin (multLin x)

prop_addMutation_NonForgetful :: LinAst -> Bool
prop_addMutation_NonForgetful x = length (exprVars $ addLin $ multLin x)
                               == length (astVars $ multLin x)
  where
    astVars :: LinAst -> [String]
    astVars (EVar n) = [n]
    astVars (ELit _) = []
    astVars (ECoeff e _) = astVars e
    astVars (EAdd e1 e2) = astVars e1 ++ astVars e2

prop_removeDup_Idempotency :: LinExpr -> Bool
prop_removeDup_Idempotency x = removeDupLin x == removeDupLin (removeDupLin x)
