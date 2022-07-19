{{=@@ @@=}}
@@name@@
=====

@@desc@@

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {@@name@@, {git, "https://host/user/@@name@@.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 @@name@@
    ===> Fetching @@name@@
    ===> Compiling @@name@@
    <Plugin Output>
