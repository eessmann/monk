# Environment snapshot framing correction

The final canonical command's two manual-baseline failures were a harness parser
defect, not translated execution differences. `DEVENV_CMDLINE` exported the
multiline invocation verbatim. Its embedded `MONK_INTEGRATION=1 cabal test ...`
line became a fabricated environment entry because the old footer emitted plain
`env` and `parseEnv` split on newlines. Duplicate keys used last-wins Map semantics,
so shell environment ordering could change the apparent delta. The old last-line
marker search was independently vulnerable to a marker embedded in a value.

Build-environment agent's controlled carrier probe is
`/tmp/monk-env-newline-probe.log`. The two new Bash/Fish runner regressions inject
a multiline value containing both a fake assignment and the former marker,
along with empty and equals-containing values, and require exact stdout plus
complete environment values. Both fail on the original parser and pass after
NUL framing. They use a small allowlisted test environment so a failure does not
dump arbitrary inherited environment content.

Only `src/Monk/Internal/Shell.hs` and `test/Unit/Harness.hs` changed. The footer now
emits a NUL-framed marker and `env -0`; splitting uses that NUL boundary and each
environment record is split only at its first equals sign. NUL cannot occur in
an environment name/value. Both canonical shells resolve env to the same Nix
GNU coreutils9.10 program at
`/nix/store/akih5l2yxpzqyh63xvyc6zsxl7kl2x4v-coreutils-9.10/bin/env`.

Build-all, focused2/2, HLint, Ormolu and diff checks pass. Logs:
`/tmp/monk-env-frame-red2.log`, `/tmp/monk-env-frame-green.log`,
`/tmp/monk-env-frame-green-build.log`. The full canonical suite reinjects the exact
multiline command from the previously failing receipt as DEVENV_CMDLINE, using
`/tmp/monk-env-frame-suite.py`; output is `/tmp/monk-env-frame-full-suite.log`.

After rebuild the actual product binary SHA256 values remain identical:

- monk: `6eb7373b7cc7d632aa01826ea6d629493d58b9fc85941b39f73987545c6434f4`
- monk-runtime: `e954672988dba4f260c9bcfb28d12bcc5c502ec826732fb90152516d625c9d91`

The verification collector's production-input fingerprint includes the internal
harness module and therefore changes; this is distinct from unchanged translator
and runtime executable bytes. Tests/bakeoff relink as expected.

Full injected-environment canonical validation completed successfully:
monk-test980/980 in63.29s; publication-test38/38 in0.10s; runtime-test passed.
No command or Cabal lock remains held. Tracked inputs frozen for final receipts.

Moving-Fish follow-up: the two new Bash/Fish snapshot regressions pass under
GHC9.14.1, Fish4.9.3, Bash5.3.9 and Nix GNUenv9.11. Stable schema2 receipt:
`final-moving-fish-harness/receipt.json`; both full and production input identities
unchanged, exit0, and monk/runtime binary hashes equal the values above. The
actual focused command completed in0.264s. No moving lane process or Cabal lock
remains. Compatibility rebuilding/testing is owned by build_environment to avoid
a duplicate concurrent build; it includes these tests in its full980 suite.
