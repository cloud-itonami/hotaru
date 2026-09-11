#!/usr/bin/env bash
# hotaru — clj/bb test suite (ADR-2606160842 py->clj port wave); ALL test namespaces, fleet green-check.
set -euo pipefail
cd "$(dirname "$0")/../.."
exec kbb -e '(require (quote clojure.test) (quote hotaru.cells.commons-readiness.test-state-machine) (quote hotaru.cells.test-state-machine) (quote hotaru.methods.test-analyze) (quote hotaru.methods.test-charter-gates))(let [r (apply clojure.test/run-tests (quote [hotaru.cells.commons-readiness.test-state-machine hotaru.cells.test-state-machine hotaru.methods.test-analyze hotaru.methods.test-charter-gates]))](System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))'
