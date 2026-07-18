(ns hotaru.cells.state-machine-test
  "State-machine tests for hotaru 蛍 cells (R0) — commons_ingest/precursor_safety/bulk_crystal_design/
  wafer_fab_design. 1:1 port of the matching portions of cells/test_state_machines.py (ADR-2606051200).
  (commons_readiness has its own test_state_machine.cljc.) .solve() raises at R0."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hotaru.cells.commons-ingest.state-machine :as ci]
            [hotaru.cells.precursor-safety.state-machine :as ps]
            [hotaru.cells.bulk-crystal-design.state-machine :as bc]
            [hotaru.cells.wafer-fab-design.state-machine :as wf]))

;; ── commons_ingest (G1/G5) ──
(defn- ingest [& {:keys [license stage cite proc] :or {license "textbook-public" stage "bulk-growth" cite "Mullin 1968" proc "p-bulk-lec"}}]
  (-> (ci/transition-to-screened {"cell_state" {} "proc_id" proc "source_license" license "stage" stage "source_cite" cite "maturity" "open-mature"})
      ci/transition-to-recorded))

(deftest test-commons-ingest-open-license-records
  (let [cs (get (ingest) "cell_state")]
    (is (= "recorded" (get cs "phase")))
    (is (= "textbook-public" (get-in cs ["payload" "sourceLicense"])))
    (is (= true (get-in cs ["payload" "screened"])))))

(deftest test-commons-ingest-accepts-edn-keyword-form
  (is (= "patent-expired" (get-in (ingest :license ":patent-expired") ["cell_state" "payload" "sourceLicense"]))))

(deftest test-commons-ingest-rejects-vendor-proprietary
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G1 violation" (ingest :license "vendor-proprietary"))))

(deftest test-commons-ingest-rejects-patent-active
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G1 violation" (ingest :license "patent-active"))))

(deftest test-commons-ingest-requires-citation
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5 violation"
                        (ci/transition-to-screened {"cell_state" {} "proc_id" "p" "source_license" "academic-oa" "stage" "wafering" "source_cite" ""}))))

(deftest test-commons-ingest-record-requires-screen-first
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"open-IP screen" (ci/transition-to-recorded {"cell_state" {}}))))

(deftest test-commons-ingest-cell-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (ci/solve {}))))

;; ── precursor_safety (G3/G4) ──
(defn- precursors [& {:keys [ph3-ack] :or {ph3-ack true}}]
  [{"name" "phosphine" "hazard_class" "acute-toxic-pyrophoric" "conflict_mineral" false "export_control" "ear" "acknowledged" ph3-ack}
   {"name" "indium" "hazard_class" "low" "conflict_mineral" true "export_control" "none" "acknowledged" true}
   {"name" "boric-oxide" "hazard_class" "benign" "conflict_mineral" false "export_control" "none" "acknowledged" true}])

(deftest test-precursor-safety-clears-clean-design
  (let [cs (get (ps/review {"cell_state" {} "design_id" "c-lec-s" "in_sourcing" "conflict-free-attested" "precursors" (precursors)}) "cell_state")]
    (is (= "cleared" (get cs "phase")))
    (is (= true (get-in cs ["payload" "cleared"])))
    (is (some #(= "ear" %) (get-in cs ["payload" "exportControls"])))))

(deftest test-precursor-safety-refuses-unverified-conflict-mineral
  (let [cs (get (ps/review {"cell_state" {} "design_id" "c-x" "in_sourcing" "unverified" "precursors" (precursors)}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G4"))))

(deftest test-precursor-safety-refuses-unacknowledged-acute-toxic
  (let [cs (get (ps/review {"cell_state" {} "design_id" "c-y" "in_sourcing" "recycled" "precursors" (precursors :ph3-ack false)}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G3"))))

(deftest test-precursor-safety-accepts-edn-keyword-sourcing
  (is (= "cleared" (get-in (ps/review {"cell_state" {} "design_id" "c-z" "in_sourcing" ":recycled" "precursors" (precursors)}) ["cell_state" "phase"]))))

(deftest test-precursor-safety-cell-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (ps/solve {}))))

;; ── bulk_crystal_design (G2/G4) ──
(defn- growth [& {:keys [method dopant in-sourcing crystal fabricated] :or {method "lec" dopant "sulfur" in-sourcing "conflict-free-attested" crystal "c-lec-s" fabricated false}}]
  (-> (bc/transition-to-screened {"cell_state" {} "crystal_id" crystal "method" method "dopant" dopant "in_sourcing" in-sourcing "target_wafer" "w-2in-n" "fabricated" fabricated})
      bc/transition-to-designed))

(deftest test-bulk-crystal-design-lec-designs
  (let [cs (get (growth) "cell_state")]
    (is (= "designed" (get cs "phase")))
    (is (= false (get-in cs ["payload" "fabricated"])))
    (is (= "lec" (get-in cs ["payload" "method"])))))

(deftest test-bulk-crystal-design-accepts-edn-keyword-method
  (let [cs (get (growth :method ":vgf" :dopant ":iron") "cell_state")]
    (is (= "vgf" (get-in cs ["payload" "method"]))) (is (= "iron" (get-in cs ["payload" "dopant"])))))

(deftest test-bulk-crystal-design-g2-refuses-fabricated-true
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2 violation" (growth :fabricated true))))

(deftest test-bulk-crystal-design-g4-refuses-unverified-sourcing
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G4 violation" (growth :in-sourcing "unverified"))))

(deftest test-bulk-crystal-design-refuses-epitaxy-method
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"bulk-growth method" (growth :method "movpe"))))

(deftest test-bulk-crystal-design-cell-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (bc/solve {}))))

;; ── wafer_fab_design (G2/spec) ──
(defn- wafer [& {:keys [diameter-um orientation epd fabricated wid] :or {diameter-um 50800 orientation "(100)" epd 5000 fabricated false wid "w-2in-n"}}]
  (-> (wf/transition-to-screened {"cell_state" {} "wafer_id" wid "diameter_um" diameter-um "orientation" orientation "epd_cm2" epd "doping" "sulfur-n" "fabricated" fabricated})
      wf/transition-to-specified))

(deftest test-wafer-fab-design-specifies
  (let [cs (get (wafer) "cell_state")]
    (is (= "specified" (get cs "phase")))
    (is (= false (get-in cs ["payload" "fabricated"])))
    (is (= 50800 (get-in cs ["payload" "diameterUm"])))))

(deftest test-wafer-fab-design-g2-refuses-fabricated-true
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2 violation" (wafer :fabricated true))))

(deftest test-wafer-fab-design-refuses-unknown-diameter
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"diameter" (wafer :diameter-um 60000))))

(deftest test-wafer-fab-design-refuses-nonpositive-epd
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"epd-cm2" (wafer :epd 0))))

(deftest test-wafer-fab-design-cell-solve-raises-at-r0
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (wf/solve {}))))
