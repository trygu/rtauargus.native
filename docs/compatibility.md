# Compatibility with INSEE's rtauargus

This package is a fork of INSEE's
[rtauargus](https://github.com/inseefrlab/rtauargus). Upstream drives the
legacy Tau-Argus executable; this fork executes the same `.arb` batches in
process through the native [tauargus-engine](https://github.com/trygu/tauargus-engine),
which is bundled in the package and built at install time.

## Preserved

- Public function names, arguments and return values, wherever the
  upstream function still applies.
- The R option names (`rtauargus.totcode`, `rtauargus.decimals`, ...).
- Datasets, vignettes and the test-suite structure.

## Intentional differences

- `run_arb` (and the table and microdata pipelines that call it) no longer
  launch `TauArgus.exe`. The batch runs in process through the bundled
  native engine; no Tau-Argus installation is needed.
- The arguments `tauargus_exe`, `logbook`, `verbose` and `...` of
  `run_arb` are kept for backward compatibility and are ignored.
- The legacy executable-path setup at load time (upstream `.onLoad`) is
  gone; the engine ships inside the package instead.
- Engine-specific logic is isolated in internal adapters
  (`R/engine_batch.R`, `R/engine_metadata.R`, `R/engine_run.R`); they are
  not part of the upstream public API.
- Adds `audit_jj(jjfile)` (the CSP intervalle audit), the engine object's
  `audit(tab)` method and the `with_audit` table export; these have no
  upstream counterpart.

## Differences from legacy Tau-Argus results

- Secondary suppression can differ cell by cell between this package
  and a legacy run. OPT and MOD solve an integer program, and when
  several suppression patterns are near-optimal, the legacy CPLEX
  backend and the HiGHS backend can pick different ones. Both patterns
  protect the table to the same standard: in the sy1 reference batch
  the two runs differ in 3 of 1654 cells (0.2%), the suppressed weight
  differs by 9 units (0.09%) and the secondary response by 149777
  (0.0016%), and the audit reports 0 unsafe cells for both patterns.
  Primary suppressions (apriori and manual) are deterministic and
  match exactly.
- WRITETABLE output types 6 (JJ) and 7 (CKM) are not implemented; the
  legacy writers for these formats were GUI-only Java with no native
  equivalent. The command fails with a clear error. Types 1 to 5
  follow the legacy argument mapping (in 0.5.4 and earlier, types 2
  to 4 were written with a single argument set that did not match
  legacy; that was fixed in 0.5.5).
