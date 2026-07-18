(ns hotaru.methods.analyze-test
  "hotaru 蛍 — analyzer tests (ADR-2606051200). 1:1 Clojure port of methods/test_analyze.py.

  Verifies the constitutional invariants empirically:
    - seed parses + classifies (G-baseline: materials/procs/crystals/wafers/precursors)
    - G1 open-license-only (and :vendor-proprietary rejected)
    - G2 design-only / not-fabricated (and fabricated=true rejected)
    - G4 conflict-mineral In/Ga detected, clean designs unflagged, :unverified flagged
    - substrate stages all open-mature; epitaxy gap; R4+ gate NOT satisfiable
    - maturity score + per-material completeness (incl. emerging-demotion)
    - safety metrics (acute-toxic / conflict-mineral / export-control)
    - gap register honesty + GaN never-mature
    - schema-conformance (validate_against_schema) + bad-value catch
    - referential integrity (process/crystal/wafer → material)
    - the 'invariant lives in THREE places' anti-drift claims (schema/lexicon/code)
    - render_report + render_datoms smoke

  All 25 Python assertions are ported 1:1. There is no datom_emit/coverage sibling in
  hotaru (the report + datoms both live in analyze), so no tests are deferred."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.string :as str]
            [clojure.set]
            [clojure.java.io :as io]
            [hotaru.methods.analyze :as A]))

(def seed (io/file "data" "seed-iii-v-substrate.kotoba.edn"))
(def lex-dir (io/file "contracts" "lexicons"))
(def onto-file (io/file "contracts" "schemas" "iii-v-substrate-ontology.kotoba.edn"))

(defn- load* []
  (A/classify (A/load-edn seed)))

(defn- first-val [m] (val (first m)))

;; lex/*.edn is now Datomic/Datascript tx-data (`[{":db/id" -1 ":lex.<name>/lexicon"
;; ... ":lex.<name>/defs" "<blob text>"}]` once parsed by A/load-edn's minimal
;; string-keyed reader, per ADR datomize fanout). `lex-doc` reverses that so the
;; three-places-agree assertions below keep reading the original bare
;; {":lexicon" ... ":id" ... ":defs" {...}} shape unchanged: strip the
;; ":lex.<name>/" namespace back to the bare key, and re-parse the ":defs" blob
;; string with A/read-edn (the same minimal reader, recursively).
(defn- lex-doc [fname]
  (A/load-edn (io/file lex-dir fname)))

(deftest test-seed-parses-and-classifies
  (let [[materials procs crystals wafers precursors] (load*)]
    (is (contains? materials "inp"))
    (is (= ":direct" (get-in materials ["inp" ":iiiv.material/bandgap-type"])))
    (is (and (>= (count procs) 16) (>= (count crystals) 3) (>= (count wafers) 2)))
    (is (and (>= (count materials) 6) (contains? materials "insb")))
    (is (and (contains? precursors "ph3") (contains? precursors "in-metal")))
    (is (true? (get-in precursors ["tmin" ":iiiv.precursor/conflict-mineral"])))))

(deftest test-g1-all-processes-open-license
  (let [[_ procs] (load*)]
    (A/screen-licenses procs)                      ; must not raise
    (doseq [p (vals procs)]
      (is (some #(= % (get p ":iiiv.proc/source-license")) A/ALLOWED-LICENSES)))))

(deftest test-g1-vendor-proprietary-is-rejected
  (let [[_ procs] (load*)
        k (key (first procs))
        bad (assoc procs k (assoc (get procs k) ":iiiv.proc/source-license" ":vendor-proprietary"))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G1 violation"
                          (A/screen-licenses bad)))))

(deftest test-g2-fabricated-true-is-rejected
  (let [[_ _ crystals wafers] (load*)
        k (key (first crystals))
        bad (assoc crystals k (assoc (get crystals k) ":iiiv.crystal/fabricated" true))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2 violation"
                          (A/screen-fabrication bad wafers)))))

(deftest test-g2-all-designs-are-not-fabricated
  (let [[_ _ crystals wafers] (load*)]
    (A/screen-fabrication crystals wafers)         ; must not raise
    (is (every? #(false? (get % ":iiiv.crystal/fabricated")) (vals crystals)))
    (is (every? #(false? (get % ":iiiv.wafer/fabricated")) (vals wafers)))))

(deftest test-substrate-stages-are-all-open-mature
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (= (get a "covered") (get a "total") 4))
    (is (true? (get a "substrate_commons_ready")))))

(deftest test-epitaxy-is-a-gap-and-r4-gate-not-satisfiable
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (>= (get-in a ["epitaxy" "n"]) 1))
    (is (false? (get-in a ["epitaxy" "open_mature"])))
    (is (false? (get a "r4_gate_satisfiable")))))

(deftest test-g4-conflict-mineral-indium-detected-and-designs-clean
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (and (contains? (get a "cm_elements") "In") (contains? (get a "cm_elements") "Ga")))
    (is (= {} (get a "cm_flagged")))))

(deftest test-g4-unverified-sourcing-is-flagged
  (let [[materials procs crystals wafers precursors] (load*)
        k (key (first crystals))
        crystals2 (assoc crystals k (assoc (get crystals k) ":iiiv.crystal/in-sourcing" ":unverified"))
        a (A/analyze materials procs crystals2 wafers precursors)]
    (is (= 1 (count (get a "cm_flagged"))))))

(deftest test-maturity-score-and-per-material-completeness
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (= 1.0 (get a "maturity_score")))
    (is (= 4 (get-in a ["per_material" "inp" "covered"])))
    (is (= 1.0 (get-in a ["per_material" "inp" "fraction"])))
    (is (= 4 (get-in a ["per_material" "gaas" "covered"])))
    (is (= 1.0 (get-in a ["per_material" "gaas" "fraction"])))
    (is (= 0.25 (get-in a ["per_material" "gan" "fraction"])))
    (is (= 1 (get-in a ["per_material" "gan" "covered"])))))

(deftest test-maturity-score-drops-when-a-stage-is-only-emerging
  (let [[materials procs] (load*)
        ;; demote both wafering processes to emerging → that stage weight 0.5 → score 0.875
        procs2 (reduce-kv (fn [m k p]
                            (assoc m k (if (= (get p ":iiiv.proc/stage") ":wafering")
                                         (assoc p ":iiiv.proc/maturity" ":open-emerging")
                                         p)))
                          (array-map) procs)
        [score _] (A/maturity-metrics procs2 materials)]
    (is (= 0.875 score))))

(deftest test-per-material-maturity-weighted-score
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (= 1.0 (get-in a ["per_material" "inp" "score"])))
    (is (= 1.0 (get-in a ["per_material" "gaas" "score"])))
    (is (= 0.125 (get-in a ["per_material" "gan" "score"])))))

(deftest test-per-material-score-reflects-emerging-demotion
  (let [[materials procs] (load*)
        procs2 (reduce-kv (fn [m k p]
                            (assoc m k (if (and (= (get p ":iiiv.proc/material") "inp")
                                                (= (get p ":iiiv.proc/stage") ":surface-prep"))
                                         (assoc p ":iiiv.proc/maturity" ":open-emerging")
                                         p)))
                          (array-map) procs)
        [_ per-material] (A/maturity-metrics procs2 materials)]
    (is (= 0.875 (get-in per-material ["inp" "score"])))
    (is (= 1.0 (get-in per-material ["gaas" "score"])))))

(deftest test-substrate-material-coverage-excludes-epitaxial-only
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)
        sm (get a "substrate_materials")]
    (is (some #(= % "ingaas") (get sm "epitaxial_only")))
    (is (= 5 (get sm "bulk_substrate")))
    (is (= 4 (get sm "full_chain")))
    (is (= #{"inp" "gaas" "gasb" "insb"} (set (get sm "full_chain_materials"))))
    (is (= 0.8 (get sm "coverage")))))

(deftest test-gasb-full-chain-and-ingaas-is-epitaxial-only
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (= 1.0 (get-in a ["per_material" "gasb" "fraction"])))
    (is (= ":epitaxial-only" (get-in materials ["ingaas" ":iiiv.material/form"])))
    (is (= 0.0 (get-in a ["per_material" "ingaas" "fraction"])))))

(deftest test-safety-metrics-export-control-and-conflict-mineral
  (let [[_ _ _ _ precursors] (load*)
        sf (A/safety-metrics precursors)]
    (is (>= (get sf "acute_toxic") 4))
    (is (>= (get sf "conflict_mineral") 4))
    (is (some #(= % "In") (get sf "conflict_mineral_formulas")))
    (is (true? (get sf "ear_present")))
    (is (false? (get sf "itar_present")))))

(deftest test-gap-register-lists-gan-and-epitaxy-honestly
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)
        gaps (get a "gaps")]
    (is (some #(and (= (get % "material") "*") (= (get % "stage") "epitaxy")) gaps))
    (let [gan-bulk (filter #(and (= (get % "material") "gan") (= (get % "stage") "bulk-growth")) gaps)]
      (is (and (seq gan-bulk) (= "emerging" (get (first gan-bulk) "status")))))
    (let [gan-absent (set (for [g gaps :when (and (= (get g "material") "gan")
                                                  (= (get g "status") "absent"))]
                            (get g "stage")))]
      (is (clojure.set/subset? #{"synthesis" "wafering" "surface-prep"} gan-absent)))
    (is (not (some #(contains? #{"inp" "gaas" "gasb" "insb"} (get % "material")) gaps)))))

(deftest test-gan-bulk-is-tracked-but-never-open-mature
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)]
    (is (= 0.8 (get-in a ["substrate_materials" "coverage"])))
    (is (not (some #(= % "gan") (get-in a ["substrate_materials" "full_chain_materials"]))))
    (is (= 1.0 (get a "maturity_score")))))

(deftest test-seed-conforms-to-ontology-allowed-sets
  (let [rows (A/load-edn seed)
        allowed (A/load-allowed-map (A/load-edn onto-file))]
    (is (contains? allowed ":iiiv.proc/source-license"))
    (is (contains? allowed ":iiiv.crystal/fabricated"))
    (is (contains? allowed ":iiiv.material/form"))
    (is (= [] (A/validate-against-schema rows allowed)))))

(deftest test-schema-validator-catches-bad-value
  (let [allowed (A/load-allowed-map (A/load-edn onto-file))
        bad [(array-map ":iiiv.proc/id" "p-x" ":iiiv.proc/maturity" ":bogus")
             (array-map ":iiiv.crystal/id" "c-x" ":iiiv.crystal/fabricated" true)]
        v (A/validate-against-schema bad allowed)
        attrs (set (map #(get % "attr") v))]
    (is (contains? attrs ":iiiv.proc/maturity"))
    (is (contains? attrs ":iiiv.crystal/fabricated"))))

(deftest test-referential-integrity-process-material-ids
  (let [[materials procs crystals wafers _] (load*)
        known (set (keys materials))]
    (doseq [[pid p] procs]
      (is (contains? known (get p ":iiiv.proc/material")) (str "process " pid " references unknown material")))
    (doseq [[cid c] crystals]
      (is (contains? known (get c ":iiiv.crystal/material")) (str "crystal " cid " references unknown material")))
    (doseq [[wid w] wafers]
      (is (contains? known (get w ":iiiv.wafer/material")) (str "wafer " wid " references unknown material")))))

;; ── the "invariant lives in THREE places" claim, machine-checked (anti-drift) ──
(defn- lstrip-colon* [x]
  (let [s (str x)]
    (if (str/starts-with? s ":") (subs s 1) s)))

(deftest test-three-places-open-license-set-agrees
  (let [onto (A/load-edn onto-file)
        schema-set (set (map lstrip-colon*
                             (get-in onto [":attributes" ":iiiv.proc/source-license" ":db/allowed"])))
        lex (lex-doc "processKnowledge.edn")
        lex-set (set (map lstrip-colon*
                          (get-in lex [":defs" ":main" ":record" ":properties" ":sourceLicense" ":enum"])))
        code-set (set (map lstrip-colon* A/ALLOWED-LICENSES))]
    (is (= schema-set lex-set code-set))))

(deftest test-three-places-fabricated-false-agrees
  (let [onto (A/load-edn onto-file)]
    (is (= [false] (get-in onto [":attributes" ":iiiv.crystal/fabricated" ":db/allowed"])))
    (is (= [false] (get-in onto [":attributes" ":iiiv.wafer/fabricated" ":db/allowed"])))
    (let [lex (lex-doc "crystalGrowthDesign.edn")]
      (is (false? (get-in lex [":defs" ":main" ":record" ":properties" ":fabricated" ":const"]))))
    (let [wlex (lex-doc "waferSpec.edn")]
      (is (false? (get-in wlex [":defs" ":main" ":record" ":properties" ":fabricated" ":const"]))))
    ;; code: screen-fabrication raises on true (the 3rd place)
    (is (thrown? clojure.lang.ExceptionInfo
                 (A/screen-fabrication (array-map "x" (array-map ":iiiv.crystal/fabricated" true))
                                       (array-map))))))

(deftest test-three-places-in-sourcing-clean-set-agrees
  (let [onto (A/load-edn onto-file)
        schema-set (set (map lstrip-colon*
                             (get-in onto [":attributes" ":iiiv.crystal/in-sourcing" ":db/allowed"])))
        lex (lex-doc "crystalGrowthDesign.edn")
        lex-set (set (map lstrip-colon*
                          (get-in lex [":defs" ":main" ":record" ":properties" ":inSourcing" ":enum"])))
        code-set (set (map lstrip-colon* A/CLEAN-SOURCING))]
    (is (= lex-set code-set))
    (is (and (clojure.set/subset? code-set schema-set) (not= code-set schema-set)))
    (is (= #{"unverified"} (clojure.set/difference schema-set code-set)))))

(deftest test-render-report-smoke
  (let [[materials procs crystals wafers precursors] (load*)
        a (A/analyze materials procs crystals wafers precursors)
        rep (A/render-report materials procs crystals wafers precursors a)]
    (is (str/includes? rep "R4+ re-evaluation gate"))
    (is (str/includes? rep "PROHIBITED through R3"))
    (let [datoms (A/render-datoms materials procs crystals wafers a)]
      (is (str/includes? datoms ":hotaru.derived/substrate-commons-ready")))))
