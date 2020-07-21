import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

-- n.b. running via `cabal v2-test` outputs plaintext, but running via
-- `cabal v2-run test:expected-fail-tests` will generate colorized
-- output.  It's adviseable to visually inspect this output to ensure
-- that "PASS (unexpected)" is rendered in Red and "FAIL (expected)"
-- is rendered in Green.

main = defaultMain $ testGroup "Expected Failures" $
  [ testCase "clearly good" $ 1 + 1 @=? 2
  , expectFail $ testCase "clearly bad" $ 1 + 1 @=? 3

  -- n.b. uncomment this to observe the results of a test that was
  -- , expectFail $ testCase "also good" $ 1 + 2 @=? 3

  , expectFail $ expectFail $ testCase "two wrongs make a right" $ 1 + 1 @=? 2
  ]
