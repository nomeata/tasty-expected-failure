{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, LambdaCase #-}
module Test.Tasty.ExpectedFailure (expectFail, expectFailBecause, ignoreTest, ignoreTestBecause, wrapTest) where

import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
import Test.Tasty ( Timeout(..) )
import Data.Typeable
import Data.Tagged
import Data.Maybe
import Data.Monoid
import Control.Exception ( displayException, try, SomeException )
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
                   }
          exceptionResult e =
            Result { resultOutcome = Failure $ TestThrewException e
                   , resultDescription = "Exception: " ++ displayException e
                   , resultShortDescription = "FAIL"
                   , resultTime = 0
                   }
      in wrap $ try (pre $ run opts t prog) >>= \case
        Right r -> return (post r)
        Left (e :: SomeException) -> return $ exceptionResult e
    testOptions = retag (testOptions :: Tagged t [OptionDescription])

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
    (testPassed "") {
      resultShortDescription = "IGNORED" <> maybe "" (mappend ": ") reason
    }
