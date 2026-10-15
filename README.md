# rtauargus.native

An R interface for [Tau-Argus](https://www.sdcweb.org/tau-argus/),
statistical disclosure control for tables and microdata.

This is a fork of INSEE's
[rtauargus](https://github.com/inseefrlab/rtauargus). Upstream launches
the legacy TauArgus.exe executable. This fork executes the same `.arb`
batches in process, through the native HiGHS-backed
[tauargus-engine](https://github.com/trygu/tauargus-engine). Function
names, arguments and return values follow upstream; see
[docs/compatibility.md](docs/compatibility.md) for the intentional
differences. The package is self-contained: the native engine and HiGHS
are bundled in it and build at install time, so there is one package to
install and no Tau-Argus installation required.

## Installation

Each GitHub Release (tags `r-v*`) contains one binary per platform
plus a source tarball. The binaries are built for R 4.5.3 (the `r4.5.3`
in the file name) and install without a compiler. The source tarball
works with other R versions and needs CMake >= 3.16 and a C++11
compiler. The CRAN dependencies (dplyr, data.table, sdcHierarchies,
...) install from CRAN as usual.

Windows:

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.5/rtauargus.native_1.3.7_r4.5.3_windows-x86_64.zip",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

macOS (Apple silicon; the release targets arm64):

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.5/rtauargus.native_1.3.7_r4.5.3_macos-arm64.tar.gz",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

Linux:

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.5/rtauargus.native_1.3.7_r4.5.3_linux-x86_64.tar.gz",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

Any platform, any R version (builds the bundled engine at install
time):

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.5/rtauargus.native_1.3.7.tar.gz",
  repos = NULL)
library(rtauargus.native)
```

Replace `r-v0.5.5` with the latest release tag.

## Quick start

Once installed, the same R code runs on Windows, macOS and Linux. The
example below protects a small table with two primary safety rules
(minimum frequency and 1-85 dominance) and the default secondary
(modular) suppression:

```r
library(rtauargus.native)

act_size <- data.frame(
  ACTIVITY = c("01","01","01","02","02","02","06","06","06","Total","Total","Total"),
  SIZE = c("tr1","tr2","Total","tr1","tr2","Total","tr1","tr2","Total","tr1","tr2","Total"),
  VAL = c(100,50,150,30,20,50,60,40,100,190,110,300),
  N_OBS = c(10,5,15,2,5,7,8,6,14,20,16,36),
  MAX = c(20,15,20,20,10,20,16,38,38,20,38,38)
)

ex1 <- tab_rtauargus(
  act_size,
  dir_name = file.path(tempdir(), "tauargus_ex1"),
  files_name = "ex1",
  explanatory_vars = c("ACTIVITY", "SIZE"),
  safety_rules = "FREQ(3,10)|NK(1,85)",
  value = "VAL",
  freq = "N_OBS",
  maxscore = "MAX",
  totcode = c(ACTIVITY = "Total", SIZE = "Total"),
  verbose = FALSE
)
ex1
```

`tab_rtauargus()` writes the full Tau-Argus input set (`.asc`, `.rda`,
`.tab`, `.arb`, logbook, CSV output) into `dir_name`, runs the batch
and returns the input table with a `Status` column:

- `A`: primary secret, frequency rule
- `B`: primary secret, dominance rule (first contributor)
- `C`: primary secret, frequency rule with more than one contributor
- `D`: secondary secret
- `V`: valid, no masking needed

## Running your own .arb batch

The quick start call already wrote the table input set (`ex1.tab` for
the values, `ex1.rda` for the metadata) into `dir_name`. You can drive
that same input set from a hand-written batch with `run_arb()`; the
legacy batch grammar is a drop-in. This batch applies the same primary
and secondary rules and exports a pivot CSV with a status column
(`AS+`):

```r
dir <- file.path(tempdir(), "tauargus_ex1")
arb <- file.path(dir, "ex1_hand.arb")
writeLines(c(
  sprintf('<OPENTABLEDATA> "%s"', file.path(dir, "ex1.tab")),
  sprintf('<OPENMETADATA> "%s"', file.path(dir, "ex1.rda")),
  '<SPECIFYTABLE> "ACTIVITY""SIZE"|"VAL"||',
  "<SAFETYRULE> FREQ(3,10)|NK(1,85)",
  "<READTABLE> 1",
  "<SUPPRESS> MOD(1,5,1,0,0)",
  sprintf('<WRITETABLE> (1,2,AS+,"%s")', file.path(dir, "ex1_hand.csv"))
), arb)

run_arb(arb)
read.csv(file.path(dir, "ex1_hand.csv"))
```

The lines follow the legacy grammar: `OPENTABLEDATA` / `OPENMETADATA`
open the input set, `SPECIFYTABLE` declares the explanatory and
response variables, `SAFETYRULE` sets the primary rules, `READTABLE`
loads the table and `SUPPRESS` runs the secondary suppression. The
four `WRITETABLE` fields are the table number, the output format
(1 plain CSV, 2 pivot CSV, 3 code value, 4 SBS, 5 intermediate),
option letters (`AS+` adds the Status column) and the output file.

`run_arb()` runs silently and returns `NULL` by default; the results
land in the output file. Pass `import = TRUE` to get a list of
data frames back into R.

To protect microdata instead of a pre-aggregated table, use
`micro_rtauargus()` (see the vignette `rtauargus_micro`).

## Checking the installed engine

```r
tauargus_engine_meta()        # engine commit + HiGHS version of this build
tauargus_version(new_tauargus())
```

Report the `tauargus_engine_meta()` output when filing a problem. The
native audit (CSP intervalle check) is exposed as `audit_jj(jjfile)`;
the front-end uses it through the engine object's `audit(tab)` method
and the `with_audit` table export.

## Ownership and development

- This repo owns the R front-end: package, tests, releases.
- The native engine and the R binding live in
  [tauargus-engine](https://github.com/trygu/tauargus-engine) and are
  pinned here as a git submodule (`tauargus-engine/`; the gitlink is
  the pin). Bump the submodule to the engine commit you want, then
  push.
- The release pipeline (`.github/workflows/release.yml`) stages the
  native layer into this package from the pinned source, builds the
  source tarball, installs it in a fresh library (the package's
  configure builds the bundled engine) and runs the full test suite on
  Linux, macOS and Windows; on `r-v*` tags it also runs a clean-room
  install of the release artifacts and cuts the GitHub Release.
