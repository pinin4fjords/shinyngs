---
name: demo-app
description: >-
  Spin up a shinyngs Shiny app on test data to demo or verify a code change.
  Use whenever asked to run the app, demo this change, show me the UI, spin up
  a test app, click through a module, or verify something in the browser for
  this repo. Covers building a throwaway R env with all deps, installing local
  changes on top, getting test data (zhangneurons or synthetic), launching a
  full app vs a single module, and the pitfalls that waste time (stale ports,
  Docker memory limits, wrapped validate() errors).
---

# Demoing shinyngs app changes

shinyngs has a heavy Bioconductor dependency tree that's slow to build from
source. The fast path is: get the deps from bioconda, install your local
changes on top, then run the app against test data.

## 1. Build the environment once

```bash
ulimit -n 1000000 && export CONDA_OVERRIDE_OSX=15.0 && \
  /opt/homebrew/bin/micromamba create -n shinyngs-dev -c conda-forge -c bioconda r-shinyngs r-testthat -y
```

This pulls the exact dependency set (Bioconductor packages, `shinyBS`,
`shinyjs`, `d3heatmap`, `plotly`, etc.) as prebuilt binaries — building them
from source directly is far slower and more failure-prone. Reuse this env
across sessions; only rebuild if bioconda's pinned version drifts far from
`DESCRIPTION`.

Activate it for every command below:

```bash
source ~/.local/bin/mm-activate shinyngs-dev
```

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

**Single module** — fastest, isolates exactly the screen you're demoing:

```r
library(shinyngs)
esel <- readRDS("data.rds")  # or however you built it in step 3
app <- prepareApp("genesetanalysistable", esel)  # module name, e.g. heatmap, pca, differentialtable
shiny::runApp(shiny::shinyApp(app$ui, app$server), host = "127.0.0.1", port = 8110, launch.browser = FALSE)
```

**Full app** — for a click-through covering navigation, or when the change
could have cross-module effects:

```r
library(shinyngs); library(shinyBS); library(shinyjs); library(markdown)
esel <- readRDS("data.rds")
app <- prepareApp("rnaseq", esel)
shiny::runApp(shiny::shinyApp(app$ui, app$server), host = "127.0.0.1", port = 8110, launch.browser = FALSE)
```

Run this backgrounded and poll for readiness rather than guessing a sleep
duration — startup time varies a lot with dataset size:

```bash
(Rscript -e "..." > /tmp/shinyngs.log 2>&1 &)
for i in $(seq 1 30); do sleep 5; \
  [ "$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 http://127.0.0.1:8110)" = "200" ] && break; done
grep -iE "listening|error|halt" /tmp/shinyngs.log | tail -5
```

Then either hand the user the URL to click through themselves, or drive it
with Playwright (see the `webapp-testing` skill) — note that shinyngs' select
inputs are `selectize.js`-enhanced, so set values via
`element.selectize.setValue(val, false)`, not by setting `.value` directly.

## Pitfalls that waste time

- **Stale process silently holding the port.** A `runApp()` left over from an
  earlier attempt (possibly hours old) can hold the port so a "restart"
  silently connects you to the *old*, unfixed app. Before every launch:
  ```bash
  for pid in $(lsof -ti tcp:8110 2>/dev/null); do kill -9 $pid; done
  ```
  and confirm the fresh log actually contains `Listening on http://...`, not
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
