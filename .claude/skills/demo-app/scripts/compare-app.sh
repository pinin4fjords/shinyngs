#!/usr/bin/env bash
# Launch two versions of a shinyngs app side by side — e.g. the base branch
# and your feature branch — against the same test data, for visual/behavioral
# comparison. See SKILL.md for the full workflow.
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: compare-app.sh OLD_REF NEW_REF MODULE DATA [OLD_PORT] [NEW_PORT]

  OLD_REF   "Before" version: a git ref (branch/tag/commit), or an existing
            checkout/worktree path.
  NEW_REF   "After" version: same options. Pass "." for your current
            checkout's working tree (including uncommitted changes).
  MODULE    shinyngs module name, or "rnaseq" for the full app — same for
            both sides, so the comparison is apples-to-apples.
  DATA      Path to an RDS file, or "zhangneurons" — same for both sides.
            If OLD_REF and NEW_REF disagree on the object's shape (e.g. a
            slot added since OLD_REF), build DATA with the OLDER version so
            the comparison also covers backward compatibility, which is
            usually the more informative direction.
  OLD_PORT  Port for the old version (default 8110)
  NEW_PORT  Port for the new version (default 8111)

Builds each version into its own R library (via build-lib.sh) so they don't
touch the shared env or each other, then launches each with run-app.sh on
its own port. Two separate R processes — required, since one R session can't
hold two versions of the same package's class definitions at once.

Example:
  compare-app.sh develop . genesetanalysistable zhangneurons
USAGE
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] || [ "$#" -lt 4 ]; then
  usage
  exit "$([ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] && echo 0 || echo 1)"
fi

OLD_REF="$1"
NEW_REF="$2"
MODULE="$3"
DATA="$4"
OLD_PORT="${5:-8110}"
NEW_PORT="${6:-8111}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKDIR="${COMPARE_WORKDIR:-/tmp/shinyngs-compare}"

echo "=== Building old version ($OLD_REF) ==="
"$SCRIPT_DIR/build-lib.sh" "$OLD_REF" "$WORKDIR/old-lib"

echo "=== Building new version ($NEW_REF) ==="
"$SCRIPT_DIR/build-lib.sh" "$NEW_REF" "$WORKDIR/new-lib"

echo "=== Launching old version on port $OLD_PORT ==="
old_status=0
R_LIB_OVERRIDE="$WORKDIR/old-lib" "$SCRIPT_DIR/run-app.sh" "$OLD_PORT" "$MODULE" "$DATA" || old_status=$?

echo "=== Launching new version on port $NEW_PORT ==="
new_status=0
R_LIB_OVERRIDE="$WORKDIR/new-lib" "$SCRIPT_DIR/run-app.sh" "$NEW_PORT" "$MODULE" "$DATA" || new_status=$?

echo "=== Summary ==="
echo "  old ($OLD_REF): http://127.0.0.1:${OLD_PORT} $([ "$old_status" -eq 0 ] && echo READY || echo FAILED)"
echo "  new ($NEW_REF): http://127.0.0.1:${NEW_PORT} $([ "$new_status" -eq 0 ] && echo READY || echo FAILED)"

[ "$old_status" -eq 0 ] && [ "$new_status" -eq 0 ]
