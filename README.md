rebar
=====

rebar is an Erlang build tool that makes it easy to compile and  
test Erlang applications, port drivers and releases.

rebar is a self-contained Erlang script, so it's easy to distribute or even  
embed directly in a project. Where possible, rebar uses standard Erlang/OTP  
conventions for project structures, thus minimizing the amount of build  
configuration work. rebar also provides dependency management, enabling  
application writers to easily re-use common libraries from a variety of  
locations (git, hg, etc).

Building
--------

Information on building and installing Erlang/OTP can be found
in the `INSTALL.md` document.

### Dependencies

To build rebar you will need a working installation of Erlang R13B03 (or
later).

Should you want to clone the rebar repository, you will also require git.

#### Downloading

Clone the git repository:

    $ git clone git://github.com/basho/rebar.git

#### Building rebar

    $ cd rebar/
    $ ./bootstrap
    Recompile: src/getopt
    ...
    Recompile: src/rebar_utils
    ==> rebar (compile)
    Congratulations! You now have a self-contained script called "rebar" in
    your current working directory. Place this script anywhere in your path
    and you can use rebar to build OTP-compliant apps.


Contributing to rebar
=====================

Indentation
-----------

To have consistent indentation we have vi modeline/emacs local variable  
headers in rebar's source files. This works automatically with vi.  
With Emacs you have to declare <code>'erlang-indent-level</code>
set to <code>4</code>  
as a safe local variable value. If not configured Emacs will prompt  
you to save this as part of custom-set-variables:

    '(safe-local-variable-values (quote ((erlang-indent-level . 4))))
You can also tell Emacs to ignore file variables:

    (setq enable-local-variables nil
          enable-local-eval nil)


Writing Commit Messages
-----------------------

One line summary (< 50 characters)
Longer description (wrap at 72 characters)

### Summary

* Less than 50 characters
* What was changed
* Imperative present tense (fix, add, change)
> Fix bug 123  
> Add 'foobar' command  
> Change default timeout to 123  
* No period

### Description

* Wrap at 72 characters
* Why, explain intention and implementation approach
* Present tense

### Atomicity

* Break up logical changes
* Make whitespace changes separately
