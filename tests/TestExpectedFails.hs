import Control.Concurrent (threadDelay)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden
import Test.Tasty.HUnit

-- n.b. running via `cabal v2-test` outputs plaintext, but running via
-- `cabal v2-run test:expected-fail-tests` will generate colorized
-- output.  It's adviseable to visually inspect this output to ensure
-- that "PASS (unexpected)" is rendered in Red and "FAIL (expected)"
-- is rendered in Green.

main = defaultMain $
  localOption (mkTimeout 1000000) $  -- 1s
  testGroup "Expected Failures" $
  [ testCase "clearly good" $ 1 + 1 @=? 2
  , expectFail $ testCase "clearly bad" $ 1 + 1 @=? 3

  -- n.b. uncomment this to observe the results of a test that was
  -- , expectFail $ testCase "also good" $ 1 + 2 @=? 3

  , expectFail $ expectFail $ testCase "two wrongs make a right" $ 1 + 1 @=? 2

  , expectFail $ testCase "throws failure" $ fail "bad"
  , expectFail $ testCase "throws error" $ error "also bad"

  , expectFail $ testCase "takes too long" $ threadDelay 2000000

  , expectFail $ goldenVsString "hello" "hello.out" $ return $ error "not golden"
  ]
