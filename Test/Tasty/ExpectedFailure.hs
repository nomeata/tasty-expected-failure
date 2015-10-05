{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Test.Tasty.ExpectedFailure
  ( expectFail
  , allowFail
  , ignoreTest
  , wrapTest
  ) where

import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Providers
import Data.Typeable
import Data.Tagged

data WrappedTest t = WrappedTest (IO Result -> IO Result) t
    deriving Typeable

instance forall t. IsTest t => IsTest (WrappedTest t) where
    run opts (WrappedTest wrap t) prog = wrap (run opts t prog)
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


-- | Marks all tests in the given test as expected failures: The tests will
-- still be run, but if they succeed, it is reported as a test suite failure,
-- and conversely a failure of the test is ignored.
--
-- Any output of a failing test is still printed.
--
-- This is useful if, in a test driven development, tests are written and
-- committed to the master branch before their implementation: It allows the
-- tests to fail (as expected) without making the whole test suite fail.
--
-- Similarly, regressions and bugs can be documented in the test suite this
-- way, until a fix is committed, and if a fix is applied (intentionally or
-- accidentally), the test suite will remind you to remove the 'expectFail'
-- marker.
expectFail :: TestTree -> TestTree
expectFail = wrapTest (fmap change)
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

-- | Marks all tests in the given test as allowed failures or successes:
-- The tests will still be run and always succeed, but the outputs are
-- reported as either allowed failure or allowed success.
--
-- This is useful when you have a 'TestTree' where some leaves are allowed
-- to fail. If you need the tests to fail when a failure is expected use
-- 'expectFail'.
allowFail :: TestTree -> TestTree
allowFail = wrapTest (fmap change)
  where
    change r
        | resultSuccessful r
        = r { resultOutcome = Success
            , resultDescription = resultDescription r `append` "(allowed success)"
            , resultShortDescription = "PASS (allowed)"
            }
        | otherwise
        = r { resultOutcome = Success
            , resultDescription = resultDescription r `append` "(allowed failure)"
            , resultShortDescription = "FAIL (allowed)"
            }
    "" `append` s = s
    t  `append` s | last t == '\n' = t ++ s ++ "\n"
                  | otherwise      = t ++ "\n" ++ s


-- | Prevents the tests from running and reports them as succeeding. This maybe
-- be desirable as an alternative comment the tests out, as they are still
-- typechecked, and the test report lists them, as a reminder that there are
-- ignored test.
ignoreTest :: TestTree -> TestTree
ignoreTest = wrapTest $ const $ return $
    (testPassed "") { resultShortDescription = "IGNORED" }
