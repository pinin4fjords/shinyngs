---
name: demo-app
description: >-
  Spin up a shinyngs Shiny app on test data to demo or verify a code change.
  Use whenever asked to run the app, demo this change, show me the UI, spin up
  a test app, click through a module, or verify something in the browser for
  this repo — including comparing before/after, old vs new, or two branches
  side by side, which this also handles by running both versions
  simultaneously on separate ports. Covers building a throwaway R env with all
  deps, installing local changes on top, getting test data (zhangneurons or
  synthetic), launching a full app vs a single module, and the pitfalls that
  waste time (stale ports, Docker memory limits, wrapped validate() errors).
---

# Demoing shinyngs app changes

shinyngs has a heavy Bioconductor dependency tree that's slow to build from
source. The fast path is: get the deps from bioconda, install your local
changes on top, then run the app against test data.

## 1. Build the environment once

```bash
micromamba create -n shinyngs-dev -c conda-forge -c bioconda r-shinyngs r-testthat -y
```

This pulls the exact dependency set (Bioconductor packages, `shinyBS`,
`shinyjs`, `d3heatmap`, `plotly`, etc.) as prebuilt binaries — building them
from source directly is far slower and more failure-prone. Reuse this env
across sessions; only rebuild if bioconda's pinned version drifts far from
`DESCRIPTION`. (`conda`/`mamba` work the same way if that's what you have
installed.)

Activate it for every command below. `micromamba activate` needs shell hook
initialization, which a fresh non-interactive shell (e.g. a tool-invoked
command) won't have unless you set it up yourself, so the self-contained form
is more reliable here:

```bash
eval "$(micromamba shell hook --shell bash)" && micromamba activate shinyngs-dev
```

If environment creation fails in a confusing way on macOS, search for known
micromamba/conda + Gatekeeper/codesign issues for your OS version before
assuming it's this package's fault — these crop up periodically and the fix
is OS-version-specific, not something to bake into this skill.

## 2. Install your local changes on top

From the repo (or worktree) checkout:

```bash
R CMD INSTALL --no-docs --no-multiarch --no-byte-compile .
```

This overwrites the bioconda-installed `shinyngs` package with your working
tree, keeping every dependency from step 1. Re-run this line after every edit
you want reflected in a running app — it's fast (a few seconds, pure R, no
compilation).

Run the test suite the same way, so internal (`@noRd`) helpers resolve
correctly against the installed package namespace:

```bash
Rscript -e 'suppressMessages({library(testthat); library(shinyngs)}); \
  test_dir("tests/testthat", env = asNamespace("shinyngs"))'
```

If the fix under test changes a function's roxygen comment (signature,
`@return`, `@description`), regenerate its `.Rd` page too. Neither
`devtools` nor `roxygen2` ships with the `r-shinyngs`/`r-testthat` env from
step 1, so `devtools::document()` fails with "there is no package called
'devtools'" — either install `roxygen2` into the env yourself
(`micromamba install -n shinyngs-dev -c conda-forge r-roxygen2` — much
lighter than pulling in all of `devtools`) and run
`roxygen2::roxygenise()`, or hand-edit the affected `man/*.Rd` block to match
(safe for a single small change; not a substitute for regenerating when
several signatures change at once).

## 3. Get test data

Pick whichever is closest to what the change touches.

**`zhangneurons` — a real, full-featured example dataset.** Best default for
demoing anything in the main RNA-seq app, since it already has contrasts,
differential stats, and gene set analyses (`roast` format) wired up.

```bash
git clone --depth 1 https://github.com/pinin4fjords/zhangneurons.git /tmp/zhangneurons
R CMD INSTALL --no-docs /tmp/zhangneurons
```

Load it in an app script with:

```r
library(zhangneurons)
data(zhangneurons, envir = environment())
# `zhangneurons` is an ExploratorySummarizedExperimentList
```

It's also useful as a **backward-compatibility check**: it predates newer
package features (e.g. slots added after it was last regenerated), so loading
it exercises any "object built with an older package version" code path for
free.

**Synthetic data via the constructors — for a narrow, targeted case.** Build
an `ExploratorySummarizedExperiment`/`List` directly in R when you need a
specific edge case (a particular column format, a `NULL` entry in a nested
list, a specific tool string) that real data won't reliably produce. See
`R/ExploratorySummarizedExperiment-class.R` for the constructor signature.

**`exec/make_app_from_files.R` — for exec/CLI changes.** Build flat CSV/TSV
inputs (sample metadata, feature metadata, an expression matrix, a contrasts
file, differential result files) and run the script directly; this is the
only way to exercise CLI-flag logic end-to-end. Check `--help` for the full
option list — it changes over time.

## 4. Launch the app

Use the bundled script for this — it kills anything already bound to the
port, launches in the background, polls for readiness (startup time varies a
lot with dataset size, so don't guess a sleep duration), and reports the log
tail. This is the part that's identical every time and where manual attempts
go wrong (see the first pitfall below), so it's worth using even for a quick
check.

The scripts live inside this skill's own folder, not the repo — use the
absolute path shown as "Base directory for this skill" when this skill was
loaded (e.g. `<skill base dir>/scripts/run-app.sh`), not a `scripts/` path
relative to the repo checkout, which won't exist there.

```bash
<skill base dir>/scripts/run-app.sh --help   # read this first
<skill base dir>/scripts/run-app.sh 8110 genesetanalysistable /tmp/data.rds   # single module — fastest, isolates one screen
<skill base dir>/scripts/run-app.sh 8110 rnaseq zhangneurons                  # full app — click-through, cross-module changes
```

`MODULE` is any shinyngs module name (`heatmap`, `pca`, `differentialtable`,
...) or `rnaseq` for the full app. `DATA` is a path to an RDS file holding an
`ExploratorySummarizedExperimentList` (however you built it in step 3), or
the literal string `zhangneurons`.

Then either hand the user the URL to click through themselves, or drive it
with Playwright (see the `webapp-testing` skill) — note that shinyngs' select
inputs are `selectize.js`-enhanced, so set values via
`element.selectize.setValue(val, false)`, not by setting `.value` directly.

Kill it when done: `lsof -ti tcp:PORT | xargs kill -9`.

## 5. Comparing two versions side by side

Sometimes the point isn't just to see the new behavior, but to see it next to
the old one — before/after a fix, or one branch against another. R can't hold
two versions of the same package's class definitions in one session, so this
needs two separate R processes, each pointed at its own private copy of
`shinyngs`, sharing everything else from the env in step 1. `compare-app.sh`
does this:

```bash
<skill base dir>/scripts/compare-app.sh --help
<skill base dir>/scripts/compare-app.sh develop . genesetanalysistable zhangneurons
#                                       ^old    ^new (this checkout, incl. uncommitted changes)
```

`OLD_REF`/`NEW_REF` are each either a git ref (branch/tag/commit) or an
existing checkout/worktree path — pass `.` for "this checkout as it is right
now." A ref that isn't currently checked out anywhere gets its own worktree
(reusing one already on that ref rather than duplicating it, and created
alongside the repo the same way any other worktree here would be); a ref
that's already checked out somewhere (very often true for `NEW_REF`, since
that's usually the branch you're already sitting in) is used directly, no
worktree needed. Each version is installed into its own library directory
under `/tmp/shinyngs-compare/` and launched via `run-app.sh` with
`R_LIB_OVERRIDE` pointed at it, so neither touches the shared env or the
other version.

`MODULE` and `DATA` are shared between both sides deliberately — same
module, same test data, different code, so any difference you see is
attributable to the change. If the two versions disagree about the shape of
that data (e.g. a slot or column was added since `OLD_REF`), build `DATA`
with the *older* version's package so the comparison also exercises backward
compatibility with it — usually the more informative direction, and it can
turn up real bugs on its own, not just cosmetic differences.

**Refreshing just one side after a local fix.** If the comparison turns up a
real bug and you fix the code, you don't need to re-run the whole
`compare-app.sh` (which reinstalls both sides) — reinstall only the side that
changed into its existing isolated library, then relaunch just that side with
`run-app.sh`, reusing the same library directory `compare-app.sh` created:

```bash
R CMD INSTALL --no-docs --no-multiarch --no-byte-compile -l /tmp/shinyngs-compare/new-lib .
R_LIB_OVERRIDE=/tmp/shinyngs-compare/new-lib <skill base dir>/scripts/run-app.sh 8111 rnaseq zhangneurons
```

The other side keeps running untouched, so the comparison stays apples-to-apples.

When done, tear everything down in one command — both apps' processes are
otherwise easy to forget since there are two of them, not one:

```bash
<skill base dir>/scripts/compare-app.sh --cleanup develop . 8110 8111
```

This kills both ports, deletes the temporary libraries, and removes any
worktree it created for `OLD_REF`/`NEW_REF` — never a path you gave directly
(like `.`), and never a worktree that already existed for other reasons; it
only touches ones at this tool's own `<repo>-compare-<ref>` naming
convention. Every normal (non-cleanup) run also prints the exact cleanup
command to copy for that invocation, so you don't have to reconstruct it
from memory.

## Pitfalls that waste time

- **Stale process silently holding the port.** A `runApp()` left over from an
  earlier attempt (possibly hours old) can hold the port so a "restart"
  silently connects you to the *old*, unfixed app — `scripts/run-app.sh`
  kills anything on the target port before launching, specifically to avoid
  this. If you launch manually instead, always do the same and confirm the
  fresh log actually contains `Listening on http://...`, not
  `createTcpServer: address already in use`.

- **`d3heatmap(matrix(1))` crashes app startup on old checkouts.** Fixed on
  `develop` (`R/apps.R`, `dendrogram = "none"`), but if you're testing a
  branch predating that fix, *every* app — not just heatmap screens — fails
  at construction with `must have n >= 2 objects to cluster`. If a full app
  won't boot at all, check this before assuming your own change broke it.

- **A `validate()` message can get rewrapped into a red error.** If a reactive
  passes an unevaluated call straight into another function's argument (e.g.
  `linkMatrix(someReactive(), ...)`), R's lazy evaluation means the
  `validate(need(...))` inside `someReactive()` doesn't fire until that
  argument is touched deep inside the callee — surfacing as something like
  `error in evaluating the argument 'x' in selecting a method for function
  'colnames'` instead of a clean validation prompt. Force the reactive to a
  local variable first. Use `options(shiny.fullstacktrace = TRUE)` to get the
  real call stack when a message looks like this.

- **Docker, if you use it instead of running locally, needs real memory.**
  The full RNA-seq app plus a large gene-set collection can OOM-kill a
  container silently (`Exited (137)`) if the Docker/colima VM has only ~2GB.
  Running directly in the local micromamba env (steps 1-2) avoids this
  entirely and is faster to iterate in; reach for Docker/`custom-studios-examples`
  only when you specifically need to validate the production container image.

- **Switching colima profiles orphans your built image.** If `docker ps`
  suddenly shows nothing and a previously-working URL goes dead, the daemon
  is pointing at a different VM — rebuild the image there, or just run
  locally instead.

- **In a comparison, `OLD_REF` failing to boot is often correct, not a
  tooling bug.** An old ref can hit a real historical bug that was later
  fixed — `compare-app.sh` still launches `NEW_REF` afterwards and reports
  both statuses, rather than stopping at the first failure. Check the
  printed log path before assuming the comparison scripts themselves are
  broken.

- **Forgetting to activate the env in any single command silently serves a
  different app, not an error.** Each tool-invoked shell command starts
  fresh — a `micromamba activate` from a previous command does not carry
  over, even mid-task. Drop the `eval "$(micromamba shell hook ...)" &&
  micromamba activate shinyngs-dev` prefix from just one `run-app.sh` /
  `R CMD INSTALL` call and R falls back to whatever `R`/`Rscript` resolves to
  on the bare PATH (a system R install, a different env, whatever's first) —
  it still launches and still answers on the port, just running stale or
  unrelated package versions with none of the fix under test. This is
  especially easy to trigger — and especially confusing — when refreshing
  one side of a running comparison (the step above): the symptom looks like
  a real regression (misbehaving JS, console errors, elements rendering
  differently) that reappeared after being fixed. Before concluding a bug
  came back, diff the loaded dependency versions between the two ports (e.g.
  compare every `link[rel=stylesheet]`/`script[src]` href via Playwright) —
  if versions that should be identical between old/new suddenly differ
  (shiny, jquery, DT, plotly...), it's a dropped activation, not a
  regression. Re-run the exact command with the activation prefix restored.

- **Playwright text-selectors can match a hidden element from a different,
  inactive tab.** shinyngs renders every module's UI into the DOM up front
  and toggles visibility per tab rather than mounting on demand, so a
  selector like `page.locator("a", has_text="help")` can resolve to a
  same-named link sitting in a tab that isn't currently displayed, then hang
  or time out on `.click()` ("element is not visible"). Scope locators to
  the visible instance, e.g. `page.locator("a.help:visible").first` or
  `.filter(has_text=...)` combined with `:visible`, rather than bare
  text-based locators.
