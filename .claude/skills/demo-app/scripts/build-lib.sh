#!/usr/bin/env bash
# Install a specific version of shinyngs into its own R library directory,
# leaving the shared env's library untouched. Used by compare-app.sh to build
# an "old" and a "new" version side by side; see SKILL.md for the workflow.
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: build-lib.sh REF_OR_PATH LIB_DIR

  REF_OR_PATH  Either an existing directory (a checkout/worktree to install
               as-is — pass "." or the current worktree's path for "my
               in-progress changes"), or a git ref (branch/tag/commit) to
               check out into a worktree first.
  LIB_DIR      Directory to install the package into (created if needed).
               Point R_LIB_OVERRIDE at this when launching the app.

When REF_OR_PATH is a ref, run this from inside any checkout of the repo.
Reuses an existing worktree already on that ref instead of creating a
duplicate (git worktree add errors if a branch is checked out twice), and
otherwise creates one alongside the repo root, matching this project's usual
worktree conventions.

Examples:
  build-lib.sh . /tmp/shinyngs-compare/new-lib          # this checkout, as-is
  build-lib.sh develop /tmp/shinyngs-compare/old-lib    # a branch/tag/commit
USAGE
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] || [ "$#" -lt 2 ]; then
  usage
  exit "$([ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ] && echo 0 || echo 1)"
fi

REF_OR_PATH="$1"
LIB_DIR="$2"

if [ -d "$REF_OR_PATH" ]; then
  SRC_DIR="$REF_OR_PATH"
  echo "==> Using existing checkout: $SRC_DIR"
else
  REPO_ROOT="$(git rev-parse --show-toplevel)"
  SAFE_REF="$(echo "$REF_OR_PATH" | tr '/' '-')"
  EXISTING="$(git -C "$REPO_ROOT" worktree list --porcelain | awk -v ref="refs/heads/$REF_OR_PATH" '
    /^worktree /{wt=$2} /^branch /{if ($2==ref) print wt}')"
  if [ -n "$EXISTING" ]; then
    SRC_DIR="$EXISTING"
    echo "==> Reusing existing worktree for '$REF_OR_PATH': $SRC_DIR"
  else
    SRC_DIR="$(dirname "$REPO_ROOT")/$(basename "$REPO_ROOT")-compare-${SAFE_REF}"
    if [ -d "$SRC_DIR" ]; then
      echo "==> Reusing previously-built worktree: $SRC_DIR"
    else
      echo "==> Creating worktree for '$REF_OR_PATH': $SRC_DIR"
      git -C "$REPO_ROOT" worktree add --detach "$SRC_DIR" "$REF_OR_PATH"
    fi
  fi
fi

mkdir -p "$LIB_DIR"
echo "==> Installing $SRC_DIR into $LIB_DIR"
R CMD INSTALL --no-docs --no-multiarch --no-byte-compile --library="$LIB_DIR" "$SRC_DIR"
