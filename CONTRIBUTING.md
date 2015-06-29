Contributing to rebar
---------------------

Before implementing a new feature, please submit a ticket to discuss your plans.
The feature might have been rejected already, or the implementation might already be decided.

See [Community and Resources](README.md#community-and-resources).

Code style
----------

The following rules must be followed:
 * Do not introduce trailing whitespace
 * Do not mix spaces and tabs
 * Do not introduce lines longer than 80 characters

The following rules should be followed:
 * Write small functions whenever possible
 * Avoid having too many clauses containing clauses containing clauses.
   Basically, avoid deeply nested functions.

[erlang-mode (emacs)](http://www.erlang.org/doc/man/erlang.el.html)
indentation is preferred. This will keep the code base consistent.
vi users are encouraged to give [Vim emulation](http://emacswiki.org/emacs/Evil) ([more
info](https://gitorious.org/evil/pages/Home)) a try.

Pull requests and branching
---------------------------

Use one topic branch per pull request. If you do that, you can add extra commits or fix up
buggy commits via `git rebase -i`, and update the branch. The updated branch will be
visible in the same pull request. Therefore, you should not open a new pull request when
you have to fix your changes.

Do not commit to master in your fork.

Provide a clean branch without merge commits.

Tests
-----

As a general rule, any behavioral change to rebar requires a test to go with it. If there's
already a test case, you may have to modify that one. If there isn't a test case or a test
suite, add a new test case or suite in `test/`.

```sh
$ ./bootstrap
$ ./rebar3 ct
```

Credit
------

To give everyone proper credit in addition to the git history, please feel free to append
your name to `THANKS` in your first contribution.

Committing your changes
-----------------------

Please ensure that all commits pass all tests, and do not have extra Dialyzer warnings.
To do that run `./rebar3 ct` and `./rebar3 as dialyze dialyzer`.

#### Structuring your commits

Fixing a bug is one commit.
Adding a feature is one commit.
Adding two features is two commits.
Two unrelated changes is two commits.

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
