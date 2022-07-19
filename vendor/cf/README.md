cf
=====

A helper library for termial colour printing extending the io:format
syntax to add colours.

```erlang
%% Effectively the same as io:format just takes the additional color
%% console text colour can be set by ~!**<colour>**.  ~#**<colour>**
%% will change the background. Both ~# only work with lowercase colours.
%% An uppercase letersindicate bold colours.
%%
%% The colour can be one of:
%%
%%   !   - resets the output
%%   ^   - bold (no colour change)
%%   __  - (two _) makes text underlined (no colour change)
%%   x,X - black
%%   r,R - red
%%   g,G - greeen
%%   y,Y - yellow
%%   b,B - blue
%%   m,M - magenta
%%   c,C - cyan
%%   w,W - white
%%
%%  The function will disable colours on non x term termials
```

Build
-----

    $ rebar3 compile


Usage
-----

`cf:format/[1,2]` - an equivalent to `io_lib:format/[1,2]`.
`cf:print/[1,2]`  - an equivalent to `io:format/[1,2]`.