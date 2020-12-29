{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, LambdaCase #-}
module Test.Tasty.ExpectedFailure (expectFail, expectFailBecause, ignoreTest, ignoreTestBecause, wrapTest) where

import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
#if MIN_VERSION_tasty(1,3,1)
import Test.Tasty.Providers.ConsoleFormat ( ResultDetailsPrinter(..) )
#endif
import Test.Tasty ( Timeout(..) )
import Data.Typeable
import Data.Tagged
import Data.Maybe
import Data.Monoid
import Control.Exception ( displayException, evaluate, try, SomeException )
import Control.Concurrent.Timeout ( timeout )


data WrappedTest t = WrappedTest (IO Result -> IO Result) t
    deriving Typeable

instance forall t. IsTest t => IsTest (WrappedTest t) where
    run opts (WrappedTest wrap t) prog =
      -- Re-implement timeouts and exception handling *inside* the
      -- wrapper.  The primary location for timeout and exception
      -- handling is in `executeTest` in the Tasty module's
      -- Test.Tasty.Run implementation, but that handling is above the
      -- level of this wrapper which therefore cannot absorb timeouts
      -- and exceptions as *expected* failures.
      let (pre,post) = case lookupOption opts of
                         NoTimeout -> (fmap Just, fromJust)
                         Timeout t s -> (timeout (faster t), fromMaybe (timeoutResult t s))
          -- 'faster' has to shorten the user-specified timeout by a
          -- small amount because the main Timeout option is
          -- separately passed to `executeTest` and that one will
          -- usually fire first if both have the same timeout.  This
          -- is a really kludgy hack, but the assumption is that most
          -- test timeouts have low resolution, so making the timeout
          -- slightly faster isn't significant in the overall sense.
          -- If it is, the test writer will hopefully find this
          -- comment, increase their timeout specification
          -- accordingly, and not curse us too heavily.
          faster t = t - 2000 -- must specify a timeout period that will *fire* faster than the original
          timeoutResult t s =
            Result { resultOutcome = Failure $ TestTimedOut t
                   , resultDescription = "Timed out after " <> s
                   , resultShortDescription = "TIMEOUT"
                   , resultTime = fromIntegral t
#if MIN_VERSION_tasty(1,3,1)
                   , resultDetailsPrinter = ResultDetailsPrinter . const . const $ return ()
#endif
                   }
          exceptionResult e =
            Result { resultOutcome = Failure $ TestThrewException e
                   , resultDescription = "Exception: " ++ displayException e
                   , resultShortDescription = "FAIL"
                   , resultTime = 0
#if MIN_VERSION_tasty(1,3,1)
                   , resultDetailsPrinter = ResultDetailsPrinter . const . const $ return ()
#endif
                   }
          forceList = foldr seq ()
      in wrap $ try (pre (run opts t prog
                         -- Ensure exceptions trying to show the
                         -- failure result are caught as "expected"
                         -- (see Issue #24 and note below)
                         >>= \r -> evaluate (forceList (resultDescription r) `seq`
                                             forceList (resultShortDescription r) `seq`
                                             resultOutcome r `seq`
                                             r)))
         >>= \case
               Right r -> return (post r)
               Left (e :: SomeException) -> return $ exceptionResult e
    testOptions = retag (testOptions :: Tagged t [OptionDescription])

    -- Note regarding post-run evaluate above:
    --
    -- The normal behavior of tasty-expected-failure is to run the
    -- test, show the failure result, but then note that the failure
    -- is expected and not count that against a test failure.  If the
    -- test unexpectedly succeeds, a message to that effect is
    -- printed, but there is no resultDescription display of the test
    -- inputs.
    --
    -- As of Tasty 1.4, the core tasty code was enhanced to fix issue
    -- #280 in tasty: essentially the test result report is forced.
    -- However, when used with tests expected to fail that also throw
    -- exceptions when attempting to show the result, the forcing in
    -- Tasty 1.4 causes an exception to be thrown after the
    -- tasty-expected-failure protections but still within the realm
    -- where tasty would count it as a failure.  The fix here attempts
    -- to `show` the failing value here in tasty-expected-failure; if
    -- an exception occurs during that `show` then code here will
    -- report it (somewhat incorrectly) via the exceptionResult above,
    -- where tasty's subsequent forcing of the text of that
    -- exceptionResult no longer causes an exception *there*.  Since
    -- the value is only shown if there was already a failure, the
    -- reason is misleading but the outcome is consistent with the
    -- intent of tasty-expected-failure handling.


-- | 'wrapTest' allows you to modify the behaviour of the tests, e.g. by
-- modifying the result or not running the test at all. It is used to implement
-- 'expectFail' and 'ignoreTest'.
wrapTest :: (IO Result -> IO Result) -> TestTree -> TestTree
wrapTest wrap = go
  where
    go (SingleTest n t) = SingleTest n (WrappedTest wrap t)
    go (TestGroup name tests) = TestGroup name (map go tests)
    go (PlusTestOptions plus tree) = PlusTestOptions plus (go tree)
    go (WithResource spec gentree) = WithResource spec (go . gentree)
    go (AskOptions f) = AskOptions (go . f)


-- | Marks all tests in the give test as expected failures: The tests will
-- still be run, but if they succeed, it is reported as a test suite failure,
-- and conversely a the failure of the test is ignored.
--
-- Any output of a failing test is still printed.
--
-- This is useful if, in a test driven development, tests are written and
-- commited to the master branch before their implementation: It allows the
-- tests to fail (as expected) without making the whole test suite fail.
--
-- Similarly, regressions and bugs can be documented in the test suite this
-- way, until a fix is commited, and if a fix is applied (intentionally or
-- accidentially), the test suite will remind you to remove the 'expectFail'
-- marker.
expectFail :: TestTree -> TestTree
expectFail = expectFail' Nothing

-- | Like 'expectFail' but with additional comment
expectFailBecause :: String -> TestTree -> TestTree
expectFailBecause reason = expectFail' (Just reason)

expectFail' :: Maybe String -> TestTree -> TestTree
expectFail' reason = wrapTest (fmap change)
  where
    change r
        | resultSuccessful r
        = r { resultOutcome = Failure TestFailed
            , resultDescription = resultDescription r <> " (unexpected success" <> comment <> ")"
            , resultShortDescription = resultShortDescription r <> " (unexpected" <> comment <> ")"
            }
        | otherwise
        = r { resultOutcome = Success
            , resultDescription = resultDescription r <> " (expected failure)"
            , resultShortDescription = resultShortDescription r <> " (expected" <> comment <> ")"
            }
    "" `append` s = s
    t  `append` s | last t == '\n' = t ++ s ++ "\n"
                  | otherwise      = t ++ "\n" ++ s
    comment = maybe "" (mappend ": ") reason

-- | Prevents the tests from running and reports them as succeeding.
--
-- This may be be desireable as an alternative to commenting out the tests. This
-- way, they are still typechecked (preventing bitrot), and the test report
-- lists them, which serves as a reminder that there are ignored tests.
--
-- Note that any setup/teardown actions executed by 'Test.Tasty.withResource'
-- are still executed. You can bypass this manually as in the following example:
--
-- @
-- askOption $ \\(MyFlag b) -> if b
--                            then withResource mytest
--                            else ignoreTest . mytest $ return junkvalue
-- @
ignoreTest :: TestTree -> TestTree
ignoreTest = ignoreTest' Nothing

-- | Like 'ignoreTest' but with additional comment
ignoreTestBecause :: String -> TestTree -> TestTree
ignoreTestBecause reason = ignoreTest' (Just reason)

ignoreTest' :: Maybe String -> TestTree -> TestTree
ignoreTest' reason = wrapTest $ const $ return $
    (testPassed $ fromMaybe "" reason) {
      resultShortDescription = "IGNORED"
    }
