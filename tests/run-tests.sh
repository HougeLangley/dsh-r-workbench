#!/usr/bin/env bash
# Run the standalone R-stage acceptance checks.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec Rscript --vanilla "$here/run-tests.R" "$@"
