import           Control.Concurrent ( threadDelay )
import           Control.Monad.IO.Class ( liftIO )
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Hedgehog

-- n.b. running via `cabal v2-test` outputs plaintext, but running via
-- `cabal v2-run test:expected-fail-tests` will generate colorized
-- output.  It's adviseable to visually inspect this output to ensure
-- that "PASS (unexpected)" is rendered in Red and "FAIL (expected)"
-- is rendered in Green.

main = defaultMain $
  localOption (mkTimeout 1000000) $  -- 1s
  testGroup "Expected Hedgehog Failures" $
  [ testProperty "good" $ property $ success
  , expectFail $ testProperty "rarely good" $ property $ do
      xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
      reverse xs === xs

  -- n.b. uncomment this to observe the results of a test that was
  -- expected to fail but actually passes.
  -- , expectFail $ testProperty "surprisingly good" $ property $ success

  , expectFail $ testProperty "giving up" $ property $ discard

  , expectFail $ expectFail $ testProperty "the failure of a failure is my good" $
    property $ success

  , expectFail $ testProperty "throws failure" $
    property $ fail "bad"

  , expectFail $ testProperty "too slow" $
    property $ do
      liftIO $ threadDelay 2000000
      success
  ]
