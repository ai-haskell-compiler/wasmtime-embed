#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"

python3 scripts/prepare-wasmtime.py all
exec cabal sdist all "$@"
