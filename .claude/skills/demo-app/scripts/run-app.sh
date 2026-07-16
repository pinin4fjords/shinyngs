#!/usr/bin/env bash
# Launch a shinyngs app for demoing/verifying a local change, deterministically:
# kill anything already bound to the port, launch in the background, poll for
# readiness, and report the log tail. See SKILL.md for the surrounding workflow
# (env setup, installing local changes, choosing test data).
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run-app.sh PORT MODULE DATA

  PORT    Localhost port to bind (e.g. 8110)
  MODULE  shinyngs module name (e.g. genesetanalysistable, heatmap, pca),
          or "rnaseq" for the full app
  DATA    Path to an RDS file holding an ExploratorySummarizedExperimentList,
          or the literal string "zhangneurons" to load the installed
          zhangneurons test package via data(zhangneurons)

Environment overrides:
  TIMEOUT_SECS   Max seconds to wait for readiness (default 150)
  POLL_INTERVAL  Seconds between readiness checks (default 5)

Requires the target R env active (SKILL.md step 1) and the local shinyngs
package already installed into it (step 2). Prints the log tail and exits 0
once the app responds 200, or exits 1 after TIMEOUT_SECS with the log tail
for diagnosis.

Examples:
  run-app.sh 8110 genesetanalysistable zhangneurons
  run-app.sh 8110 rnaseq /tmp/my-test-object.rds
USAGE
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] || [ "$#" -lt 3 ]; then
  usage
  exit "$([ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] && echo 0 || echo 1)"
fi

PORT="$1"
MODULE="$2"
DATA="$3"
LOG="$(mktemp -t "shinyngs-app-${PORT}-XXXXXX").log"
TIMEOUT_SECS="${TIMEOUT_SECS:-150}"
POLL_INTERVAL="${POLL_INTERVAL:-5}"

echo "==> Killing any process already bound to port $PORT"
for pid in $(lsof -ti tcp:"$PORT" 2>/dev/null || true); do
  kill -9 "$pid" 2>/dev/null || true
done
sleep 1

if [ "$DATA" = "zhangneurons" ]; then
  load_expr='suppressMessages(library(zhangneurons)); data(zhangneurons, envir = environment()); esel <- zhangneurons'
else
  if [ ! -f "$DATA" ]; then
    echo "==> DATA file not found: $DATA" >&2
    exit 1
  fi
  load_expr="esel <- readRDS(\"$DATA\")"
fi

r_expr="suppressMessages({library(shinyngs); library(shinyBS); library(shinyjs); library(markdown)}); ${load_expr}; app <- prepareApp(\"${MODULE}\", esel); shiny::runApp(shiny::shinyApp(app\$ui, app\$server), host = \"127.0.0.1\", port = ${PORT}, launch.browser = FALSE)"

echo "==> Launching module '$MODULE' on port $PORT (log: $LOG)"
(Rscript -e "$r_expr" >"$LOG" 2>&1 &)

echo "==> Waiting up to ${TIMEOUT_SECS}s for readiness"
elapsed=0
ready=false
while [ "$elapsed" -lt "$TIMEOUT_SECS" ]; do
  sleep "$POLL_INTERVAL"
  elapsed=$((elapsed + POLL_INTERVAL))
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 "http://127.0.0.1:$PORT" || echo 000)
  if [ "$code" = "200" ]; then
    ready=true
    break
  fi
done

echo "==> Log tail:"
grep -iE "listening|error|halt|execution" "$LOG" | tail -10 || true

if [ "$ready" = true ]; then
  echo "==> READY after ~${elapsed}s: http://127.0.0.1:${PORT}"
  exit 0
else
  echo "==> NOT READY after ${TIMEOUT_SECS}s — check $LOG"
  exit 1
fi
