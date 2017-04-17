{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Test.Tasty.ExpectedFailure (expectFail, ignoreTest, wrapTest) where

import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
import Data.Typeable
import Data.Tagged
import Control.Exception(SomeException, catch)

data WrappedTest t = WrappedTest (IO Result -> IO Result) t
    deriving Typeable

instance forall t. IsTest t => IsTest (WrappedTest t) where
    run opts (WrappedTest wrap t) prog = wrap (run opts t prog)
    testOptions = retag (testOptions :: Tagged t [OptionDescription])

-- | 'wrapTest' allows you to modify the behavoiur of the tests, e.g. by
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
expectFail = wrapTest (fmap change . (`catch` (return . exceptionResult)))
  where
    change r
        | resultSuccessful r
        = r { resultOutcome = Failure TestFailed
            , resultDescription = resultDescription r `append` "(unexpected success)"
            , resultShortDescription = "PASS (unexpected)"
            }
        | otherwise
        = r { resultOutcome = Success
            , resultDescription = resultDescription r `append` "(expected failure)"
            , resultShortDescription = "FAIL (expected)"
            }
    "" `append` s = s
    t  `append` s | last t == '\n' = t ++ s ++ "\n"
                  | otherwise      = t ++ "\n" ++ s

-- Copied from Test.Tasty.Core
-- | Shortcut for creating a 'Result' that indicates exception
exceptionResult :: SomeException -> Result
exceptionResult e = Result
  { resultOutcome = Failure $ TestThrewException e
  , resultDescription = "Exception: " ++ show e
  , resultShortDescription = "FAIL"
  , resultTime = 0
  }


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
ignoreTest = wrapTest $ const $ return $
    (testPassed "") { resultShortDescription = "IGNORED" }
