(ns hotaru.cells.commons-readiness.test-state-machine
  "clojure.test port of the commons_readiness assertions from
  `cells/test_state_machines.py` (hotaru 蛍, ADR-2606051200).

  Ports ONLY the five commons_readiness cases (the `_readiness` helper +
  test_commons_readiness_*). The Python file ALSO covers the OTHER hotaru cells'
  state machines — those are DEFERRED (their `state_machine.py` are not yet
  ported to .cljc):
    - commons_ingest    (7 cases: open-license / edn-keyword / vendor-proprietary
                         / patent-active / citation / record-before-screen / .solve raise)
    - precursor_safety  (5 cases: clean-clear / conflict-mineral / acute-toxic /
                         edn-keyword / .solve raise)
    - bulk_crystal_design (6 cases: lec-designs / edn-keyword / fabricated-true /
                           unverified-sourcing / epitaxy-method / .solve raise)
    - wafer_fab_design  (5 cases: specifies / fabricated-true / unknown-diameter /
                         nonpositive-epd / .solve raise)
    - the .solve()-raises case for the CommonsReadinessCell itself (cell.py, not
      state_machine.py — the cell class is not part of the .cljc port surface)."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [hotaru.cells.commons-readiness.state-machine :as sm]))

;; ── helper (port of the Python `_readiness`) ─────────────────────

(def ^:private full-substrate
  {"synthesis" "open-mature" "bulk-growth" "open-mature"
   "wafering" "open-mature" "surface-prep" "open-mature"})

(defn- readiness
  "Port of `_readiness(per_stage, epitaxy, conflict, **extra)`: assess then
  report. `extra` is a map merged onto the top-level state (e.g. a forbidden
  adjudicating key for the G3 test)."
  [& {:keys [per-stage epitaxy conflict extra]
      :or {per-stage full-substrate epitaxy false conflict 0 extra {}}}]
  (let [s (sm/transition-to-assessed
           (merge {:cell-state {} :per-stage per-stage
                   :epitaxy-open-mature epitaxy :conflict-flagged conflict}
                  extra))]
    (sm/transition-to-reported s)))

;; ── commons_readiness (G3) ───────────────────────────────────────

(deftest test-commons-readiness-full-substrate-scores-one
  (let [cs (:cell-state (readiness))
        p (:payload cs)]
    (is (= "reported" (:phase cs)))
    (is (= 1.0 (get p "maturityScore")))
    (is (true? (get p "substrateCommonsReady")))
    ;; epitaxy gap → R4+ gate not satisfiable; fabrication stays prohibited
    (is (false? (get p "r4GateSatisfiable")))
    (is (true? (get p "fabricationProhibited")))))

(deftest test-commons-readiness-emerging-stage-lowers-score
  (let [cs (:cell-state
            (readiness :per-stage {"synthesis" "open-mature" "bulk-growth" "open-mature"
                                   "wafering" "open-emerging" "surface-prep" "open-mature"}))
        p (:payload cs)]
    (is (= 0.875 (get p "maturityScore")))
    (is (false? (get p "substrateCommonsReady")))))

(deftest test-commons-readiness-g3-refuses-adjudicating-key
  (testing "an adjudicating input key raises (G3)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G3 violation"
                          (readiness :extra {"gateOpened" true})))))

(deftest test-commons-readiness-r4-satisfiable-only-with-open-epitaxy
  (let [cs (:cell-state (readiness :epitaxy true))
        p (:payload cs)]
    (is (true? (get p "r4GateSatisfiable")))
    ;; even when the commons would satisfy the gate, the report never opens fabrication
    (is (true? (get p "fabricationProhibited")))))

;; ── extra parity guards on the ported logic ──────────────────────

(deftest test-commons-readiness-accepts-edn-keyword-maturity
  ;; _norm strips a leading ':' so EDN keyword-form maturity reads as the bare string
  (let [cs (:cell-state
            (readiness :per-stage {"synthesis" ":open-mature" "bulk-growth" ":open-mature"
                                   "wafering" ":open-mature" "surface-prep" ":open-mature"}))]
    (is (= 1.0 (get (:payload cs) "maturityScore")))
    (is (true? (get (:payload cs) "substrateCommonsReady")))))

(deftest test-commons-readiness-absent-stage-zero-weight
  (let [cs (:cell-state
            (readiness :per-stage {"synthesis" "open-mature" "bulk-growth" "gap"}))
        p (:payload cs)]
    ;; (1.0 + 0.0 + absent 0.0 + absent 0.0) / 4 = 0.25
    (is (= 0.25 (get p "maturityScore")))
    (is (= 1 (get p "stagesCovered")))
    (is (= 4 (get p "stagesTotal")))
    (is (false? (get p "substrateCommonsReady")))))

(deftest test-commons-readiness-unknown-maturity-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown maturity"
                        (readiness :per-stage {"synthesis" "bogus"}))))

(deftest test-commons-readiness-report-requires-assessment-first
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires an assessment first"
                        (sm/transition-to-reported {:cell-state {}}))))

(deftest test-commons-readiness-conflict-flagged-passthrough
  (let [cs (:cell-state (readiness :conflict 3))]
    (is (= 3 (get (:payload cs) "conflictFlagged")))))

#?(:clj
   (defn -main [& _]
     (run-tests 'hotaru.cells.commons-readiness.test-state-machine)))
