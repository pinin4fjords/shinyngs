#!/usr/bin/env bash
# Install a specific version of shinyngs into its own R library directory,
# leaving the shared env's library untouched. Used by compare-app.sh to build
# an "old" and a "new" version side by side; see SKILL.md for the workflow.
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: build-lib.sh REF_OR_PATH LIB_DIR
       build-lib.sh --resolve REF_OR_PATH

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

--resolve prints the checkout path that would be used for REF_OR_PATH,
without creating a worktree or installing anything — used by
compare-app.sh --cleanup to find (and only remove) worktrees it created,
without side effects from just asking.

Examples:
  build-lib.sh . /tmp/shinyngs-compare/new-lib          # this checkout, as-is
  build-lib.sh develop /tmp/shinyngs-compare/old-lib    # a branch/tag/commit
  build-lib.sh --resolve develop
USAGE
}

# Prints the checkout path for REF_OR_PATH to stdout. If CREATE=true and no
# worktree exists yet, creates one; if CREATE=false, only reports the path
# a subsequent build would use, without touching the filesystem or git state.
# All diagnostics go to stderr so stdout is just the path, safe to capture.
resolve_src_dir() {
  local ref_or_path="$1" create="$2"

  if [ -d "$ref_or_path" ]; then
    echo "==> Using existing checkout: $ref_or_path" >&2
    echo "$ref_or_path"
    return
  fi

  local repo_root safe_ref existing src_dir
  repo_root="$(git rev-parse --show-toplevel)"
  safe_ref="$(echo "$ref_or_path" | tr '/' '-')"
  existing="$(git -C "$repo_root" worktree list --porcelain | awk -v ref="refs/heads/$ref_or_path" '
    /^worktree /{wt=$2} /^branch /{if ($2==ref) print wt}')"

  if [ -n "$existing" ]; then
    echo "==> Reusing existing worktree for '$ref_or_path': $existing" >&2
    echo "$existing"
    return
  fi

  src_dir="$(dirname "$repo_root")/$(basename "$repo_root")-compare-${safe_ref}"
  if [ -d "$src_dir" ]; then
    echo "==> Reusing previously-built worktree: $src_dir" >&2
  elif [ "$create" = true ]; then
    echo "==> Creating worktree for '$ref_or_path': $src_dir" >&2
    git -C "$repo_root" worktree add --detach "$src_dir" "$ref_or_path" >&2
  fi
  echo "$src_dir"
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

if [ "${1:-}" = "--resolve" ]; then
  [ "$#" -lt 2 ] && { usage; exit 1; }
  resolve_src_dir "$2" false
  exit 0
fi

if [ "$#" -lt 2 ]; then
  usage
  exit 1
fi

REF_OR_PATH="$1"
LIB_DIR="$2"

SRC_DIR="$(resolve_src_dir "$REF_OR_PATH" true)"

mkdir -p "$LIB_DIR"
echo "==> Installing $SRC_DIR into $LIB_DIR"
R CMD INSTALL --no-docs --no-multiarch --no-byte-compile --library="$LIB_DIR" "$SRC_DIR"
