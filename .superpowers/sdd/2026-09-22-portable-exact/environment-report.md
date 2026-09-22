# Optimized final handoff

Fresh canonical980+allnative+publication10 pass in final-optimized-runtime; GHC9.12 focused rebuild/checks and movingFish focused checks pass. Compiler6eb737... unchanged, runtime069d8c... verified. New Darwin package final-package-optimized passes copiedcleanenvironment suites, signedruntime76e2ba5b..., archive49571f0d... at artifacts/monk-runtime-aarch64-darwin-optimized.tar.gz. All prior package/rawmeasurements retained.

Fresh default74/stable77 comparison counts unchanged. Optimized performance common14 baseline171.476ms vs candidate621.447ms =3.624x; preoptimization6.545x. Improvement does not pass performance: substantial regression persists, common16 incomplete, arithmetic3missing, targeted4unavailable, process launchesunverified. Helper-free criterion remains unmet; Linuxexecution/minOS execution unverified.

Durable final report: docs/design/portable-runtime-verification.md. Ledger: docs/evidence/portable-exact-final-verification-2026-09-22.json. Final sourcearchive content bridge explicitly reuses prior whole-sdist evidence plus newruntime builds, without claiming a whole-finalarchive rebuild.

# Final environment and evidence handoff

Final evidence: docs/design/portable-runtime-verification.md and docs/evidence/portable-exact-final-verification-2026-09-22.json.

Canonical GHC9.14.1 and compatibility9.12.2 main980 pass. Moving Fish4.9.3 full978 plus2 harness regressions pass. All native suites, callback33, managed launcher all8stdio masks, publication10, API12, quality27, Haddock and unpacked sdist build/install pass as recorded. The failed canonical receipt retains its obsolete publication assertion; focused followup is stable and green. Product SHA256 hashes unchanged through the NUL test-harness correction.

Default historic95 is74matches/21rejections; stable-directory77/18, neither has admitted mismatch. Separate default effect4+strengthened4matches/time1rejection retain denominators. Darwin runtime signed SHA4dda9b6e... and portable archive65eb8b0c... are in artifacts/runtime-final and artifacts/monk-runtime-aarch64-darwin.tar.gz.

Performance is not accepted: surviving common14 is6.545x baseline median time; common16 incomplete, arithmetic3 inputs missing, targeted4 unavailable. Linux native execution user-deferred; process-launch tracing and minimumOS execution unverified. Helper-free greeting/conditional criterion remains unmet by exact writer+native launcher. No remote execution, pushes or commits.

Final source archive content bridge and documentation closure are stored in artifacts and the documentation-closure receipt after doc freeze.
