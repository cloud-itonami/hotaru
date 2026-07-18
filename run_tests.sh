#!/usr/bin/env bash
# hotaru standalone Clojure test suite.
set -euo pipefail
cd "$(dirname "$0")"
exec clojure -M:test
