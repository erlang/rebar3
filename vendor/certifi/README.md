# [certifi](https://github.com/certifi/erlang-certifi)

[![Build Status](https://github.com/certifi/erlang-certifi/workflows/build/badge.svg)](https://github.com/certifi/erlang-certifi)

This Erlang library contains a CA bundle that you can reference in your Erlang
application. This is useful for systems that do not have CA bundles that
Erlang can find itself, or where a uniform set of CAs is valuable.

This an Erlang specific port of [certifi](https://certifi.io/). The CA bundle
is derived from Mozilla's canonical set.

List of included certificates:
https://ccadb-public.secure.force.com/mozilla/IncludedCACertificateReport

## Usage

```erlang
CaCerts = certifi:cacerts(),
SslOptions = [{verify, verify_peer},
              {depth, 99},
              {cacerts, CaCerts}],
ssl:connect( "example.com", 443, SslOptions ).
```


You can also retrieve the path to the file and load it by yourself if needed:

```erlang
Path = certifi:cacertfile().
```

## Build & test

```shell
$ rebar3 eunit
```
