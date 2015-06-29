Contributing to rebar
---------------------

Before implementing a new feature, please submit a ticket to discuss your plans.
The feature might have been rejected already, or the implementation might already be decided.

See [Community and Resources](README.md#community-and-resources).

Code style
----------

The following rules apply:
 * Do not introduce trailing whitespace
 * We use spaces for indenting only
 * Try not to introduce lines longer than 80 characters
 * Write small functions whenever possible
 * Avoid having too many clauses containing clauses containing clauses.
   Basically, avoid deeply nested functions.

Follow the indentation style of existing files. The [erlang-mode for
(emacs)](http://www.erlang.org/doc/man/erlang.el.html) indentation is going to
always work. Other users may want to use 4-width spaces and make sure things
align mostly the way they would with Emacs code, or with the rest of the
project.

Where possible, include type specifications for your code so type analysis
will be as accurate as possible.

Please add comments around tricky fixes or workarounds so that we can
easily know why they're there at a glance.

Pull requests and branching
---------------------------

All fixes to rebar end up requiring a +1 from one or more of the project's
maintainers. When opening a pull request, explain what the patch is doing
and if it makes sense, why the proposed implementation was chosen.

Try to use well-defined commits (one feature per commit) so that reading
them and testing them is easier for reviewers and while bissecting the code
base for issues.

During the review process, you may be asked to correct or edit a few things
before a final rebase to merge things.

Please work in feature branches, and do not commit to `master` in your fork.

Provide a clean branch without merge commits.

Tests
-----

As a general rule, any behavioral change to rebar requires a test to go with
it. If there's already a test case, you may have to modify that one. If there
isn't a test case or a test suite, add a new test case or suite in `test/`.

To run the tests:

```sh
$ ./bootstrap
$ ./rebar3 ct
```

The rebar3 test suite is written using Common Test. As many of the tests as
possible are written by using the programmatic rebar3 API rather than
by running the escriptized project directly. The tests should be restricted
to their `priv_dir` and leave the system clean after a run.

 If such tests prove hard to write, you can ask for help doing that in your
pull request.

For tests having a lot to do with I/O and terminal interaction, consider
adding them to https://github.com/tsloughter/rebar3_tests


Credit
------

To give everyone proper credit in addition to the git history, please feel free to append
your name to `THANKS` in your first contribution.

Committing your changes
-----------------------

Please ensure that all commits pass all tests, and do not have extra Dialyzer warnings.
To do that run `./rebar3 ct` and `./rebar3 as dialyze dialyzer`.

#### Structuring your commits

- Fixing a bug is one commit.
- Adding a feature is one commit.
- Adding two features is two commits.
- Two unrelated changes is two commits (and likely two Pull requests)

If you fix a (buggy) commit, squash (`git rebase -i`) the changes as a fixup commit into
the original commit.

#### Writing Commit Messages

It's important to write a proper commit title and description. The commit title must be
at most 50 characters; it is the first line of the commit text. The second line of the
commit text must be left blank. The third line and beyond is the commit message. You
should write a commit message. If you do, wrap all lines at 72 characters. You should
explain what the commit does, what references you used, and any other information
that helps understanding your changes.

Basically, structure your commit message like this:

<pre>
One line summary (at most 50 characters)

Longer description (wrap at 72 characters)
</pre>

##### Commit title/summary

* At most 50 characters
* What was changed
* Imperative present tense (Fix, Add, Change)
 * `Fix bug 123`
 * `Add 'foobar' command`
 * `Change default timeout to 123`
* No period

##### Commit description

* Wrap at 72 characters
* Why, explain intention and implementation approach
* Present tense
