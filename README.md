tasty-expected-failure
======================

What is this?
-------------

With the function `expectFail` in the provided module
`ExpectedFailure`, you can mark that you expect test cases to fail,
and not to pass.

This can for example be used for test-driven development: Create the tests,
mark them with `expectFail`, and you can still push
to the main branch, without your continuous integration branch failing.

Once someone implements the feature or fixes the bug (maybe unknowingly), the
test suite will tell him so, due to the now unexpectedly passing test, and he
can remove the `expectFail` marker.

The module also provides `ignoreTest` to avoid
running a test. Both funtions are implemented via the more general
`wrapTest`, which is also provided.

Why is this not provided by tasty?
----------------------------------

`<rant>`

The author of the tasty library prefers to provide a minimal experience in the
tasty library, instead of a batteries-included approach, and chose not to
include these 39 lines of code in tasty. See the
[issue](https://github.com/feuerbach/tasty/issues/114) for the discussion.

Instead I wrote 37 lines of cabal file, a similar number of lines of README,
created a git repository, created a travis file, run travis to figure out on
what versions it builds (something that would have happened automatically with
a pull request for tasty), upload to hackge, add to stackage.

Furthermore, there is little discoverability: If it were part of the tasty API,
users would stumble over it. Now they likely won’t. And if they do, they have
to worry about whether it is still in sync with `tasty`, they have to add it to
their build-depends, they have to import yet another module. Distribution
packagers will have yet another package where they have to create the
packaging, check the copyright, and run autobuilders for.

Sigh.

`</rant>`
