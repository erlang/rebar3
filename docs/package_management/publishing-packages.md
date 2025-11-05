# Publishing Packages

For package management `rebar3` uses [hex.pm](https://hex.pm), a package manager for Erlang and Elixir packages.

## Basic usage

You should read the [docs](https://hexdocs.pm/rebar3_hex/) for a complete guide for each provider, but below is a
brief overview of basic usage.

### Registering a Hex user

When registering a user, you will be prompted for a username, your email and a password. The email is used to confirm your identity during signup, as well as to contact you in case there is an issue with one of your packages. The email will never be shared with a third party.

```nohighlight
$ rebar3 hex user register
Username: johndoe
Email: john.doe@example.com
Password:
Password (confirm):
Registering...
Generating API key...
You are required to confirm your email to access your account, a confirmation email has been sent to john.doe@example.com

```

### Authenticating User

If you already have a user for [hex.pm](https://hex.pm) run:


```shell
$ rebar3 hex user auth
```

Note that this will ask for your hex.pm username and password, as well as a password for encrypting your api token that
has write permissions to the repository. When publishing a package you will have to give this password to decrypt the
token in order to publish.

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_user.html) for more information.

### Building

The `build` provider is very useful for seeing exactly what would be published using either the `publish` or `cut` task
without any chance of actually publishing the package or docs. Tarballs for the package and docs are written to
`_build/<profile>/lib/your_app/hex/` by default.


```
$ rebar3 hex build
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_build.html) for more information.

### Publishing

Two providers are available for publishing packages, `publish` and `cut`

By default `publish` builds and pushes your package and docs to a repository. See the
[docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_publish.html) for more information.

``` shell
$ rebar3 hex publish
```

`cut` is available to provide some additional functionality around versioning and git tags. See the
[docs](https://hexdocs.pm/hex/rebar3_hex_cut.html) for more information.

``` shell
$ rebar3 hex cut
```

In either case, both providers will display the details of what is being published
(what files, the version, dependencies) and ask if it should continue, so be sure to read the
output carefully and make sure it is publishing what you expected.

### Managing package owners

Owners can be added, removed, and listed for packages you are an owner of with the `hex owner` command. Packages
can also be transfered to other registered users on hexpm as well.

``` shell
$ rebar3 hex owner [add | remove | list | transfer] <package> <email>
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_owner.html) for more information.

### Retiring packages

Packages can be flagged as retired on hexpm via the `retire` provider :

```
$ rebar3 hex retire PACKAGE VERSION REASON --message
```

They can also be unretired in case a mistake was made :

```
$ rebar3 hex retire PACKAGE VERSION --unretire
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_retire.html) for more information.

### Organizations

rebar3_hex supports working with organizations via the `organization` provider.

Simply add your organization to either your global rebar.config (i.e., `~/.config/rebar3/rebar.config` ) or a local
project rebar.config.

```erlang
{hex, [{repos, [#{name => <<"hexpm:your_org">>}]}]}.
```

You can then authenticate with with the organization repository. Be sure you have already authenticated with the main
repository first:

```
$ rebar3 hex auth    # make sure you're authenticated to the main repo first
$ rebar3 hex organization auth hexpm:your_org  # authenticate to the org
```

Now you can generate, revoke, and list keys for your organizations. See below for an example of generating a key for use
in CI.

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_organization.html) for more information.

#### Read-Only Repo Key for CI

If you have a private organization or other private repository it is recommended that you use a repo specific
auth token for reading from the repository in CI. To generate a token:

```shell
$ rebar3 hex organization auth hexpm:myrepo
Successfully authenticated to hexpm:myrepo
```

Now you can generate a key to use in CI for your organization

```
$ rebar3 hex organization key hexpm:myrepo generate
abc123
```

Then in CI use whatever method is available for setting an environment variable to the token and add this call at the
beginning of your CI runs to add the token to your rebar3 hex config. Below we'll use the environment `REPO_KEY` as an
example.

```shell
$ rebar3 hex organization auth hexpm:myrepo --key $REPO_KEY
```

### Searching hexpm

A `search` provider is available to search packages across hex.pm as a convenience.

```
$ rebar3 hex search
```

## Further reading
See [Publishing packages](https://hex.pm/docs/rebar3-publish) on hexpm for more setup and usage instructions. See the
[docs](https://hexdocs.pm/rebar3_hex) for detailed documentation for all available providers.
