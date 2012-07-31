Ducttape seeks to address the following issues from LoonyBin:

1) Long edit, recompile, regenerate, copy, re-run cycles
2) Confusing Python->Bash code generation
3) GUI testing/support/development headache
4) Relocatable workflows (no absolute symlinks)
5) SSH spamming (no remote workflows, for now)
6) Multi-pass post-analysys (workflows can now be re-analyzed using "Summaries" long after the workflow was originally written)
7) Share status pages via a central web server
8) Mediate complexity with subworkflows and groups
9) Multiple levels of escaping with bash/python/SSH/heredocs are gone
10) Using workflows as templates via config files
11) Collaborative workflows vai reading an upstream workflow as a type of subworkflow (future work)
12) Scriptable via CLI
13) Realizatoin/file globbing
14) Simple one-off experiments can be added in a single line
15) Software versioning via Git/SVN and file versioning via SHA1/MD5
16) "Local build and copy" pattern for obstinant remote build environments (e.g. cluster workers vs gateways, or just bad gateways)
17) Parameter dependencies don't affect temporal ordering

* The overall design now uses a multi-pass strategy rather than a single pass strategy.

* TODO: How would we handle a factored translation setup?
