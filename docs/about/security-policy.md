# Security Policy
## Security Caveats

Rebar3 is a build tool that by design allows arbitrary code execution from downloaded components. Scripts can be executed in all kinds of areas of a regular project workflow including (but not limited to): scripts to modify configuration files, "parse transforms" (macros), plugins, provider and shell hooks, and so on.

Users of Rebar3 should be aware of the nature of its model, and issues related to these parts of its design will not be considered to be security issues nor vulnerabilities.

## Reporting a Security Issue

All security issues should be reported to one or more of the current maintainers:

- [Fred Hebert](https://keybase.io/mononcqc) ([@ferd](https://github.com/ferd/))
- [Tristan Sloughter](https://keybase.io/tsloughter) ([@tsloughter](https://github.com/tsloughter/))

E-Mail addresses are available in GitHub profiles, and PGP public keys in Keybase profiles.

If you have not received a reply to your query within 48 hours, or have not heard from one of the maintainers for the past five days, there are a few steps you can take:

- One of the authenticated channels in the maintainers Keybase profiles
- Open a GitHub issue directly
- Ask on #rebar3 on the [official Erlang Slack team](https://erlef.org/slack-invite/erlanger)
- Ask on #rebar on IRC on libera.chat

Please note that the GitHub issues, mailing list, and chat channels are public areas. When escalating in these venues, please do not discuss details of your issue. Simply say that you’re trying to get a hold of someone from the maintainer team.

## Disclosure Policy

We're a small project of volunteers working in whatever free time they have, with limited mechanisms to reach developers from other communication channels.

Disclosure will be fairly ad-hoc and made to reach as many people as possible.

Nevertheless, the expected steps are:

1. The issue is received and discussed privately by the maintainers
2. A fix is prepared and reviewed between maintainers
3. When ready, the fix will be committed to the repository and a release will be cut
4. An announcement will be made about the new release on the public channels associated to the project

## Receiving Security Updates

The best way to know about security updates is to subscribe to any of the communication channels of the project.

## Comments on This Policy

If you have any suggestions to improve this policy, please contact the maintainers or open a GitHub issue.
