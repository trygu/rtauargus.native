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
