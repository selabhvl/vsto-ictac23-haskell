import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import Experiments
import TestRenameIssue

main :: IO ()
main = hspec $ do
  describe "Running some HUnit tests" $ do
    fromHUnitTest tests_debugging
    -- fromHUnitTest tests_equal
    fromHUnitTest tests_smallFlatProblem
    fromHUnitTest tests_equal_all_plans
