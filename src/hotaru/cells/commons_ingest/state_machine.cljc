(ns hotaru.cells.commons-ingest.state-machine
  "Phase state machine for the hotaru 蛍 commons_ingest cell.
  1:1 port of cells/commons_ingest/state_machine.py (ADR-2606051200). A process datom becomes an
  open-publication record ONLY after its source-license passes the open-IP screen (G1) and carries a
  primary-source citation (G5). Conventions: dataclass → plain map (Python string keys); ValueError → ex-info."
  (:require [clojure.string :as str]))

(def allowed-licenses #{"academic-oa" "patent-expired" "textbook-public" "standard-public" "own-rnd"})
(def allowed-stages #{"synthesis" "bulk-growth" "wafering" "surface-prep" "epitaxy"})

(def state-defaults
  {"phase" "init" "proc_id" "" "stage" "bulk-growth" "source_license" "academic-oa"
   "source_cite" "" "maturity" "open-emerging" "screened" false "sourcing" "representative" "payload" {}})

(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))

(defn transition-to-screened [state]
  (let [cs (cell-state state)
        cs (assoc cs "proc_id" (get state "proc_id" (get cs "proc_id")))
        lic (norm (get state "source_license" (get cs "source_license")))]
    (when-not (contains? allowed-licenses lic)
      (throw (ex-info (str "G1 violation: process " (pr-str (get cs "proc_id")) " has source-license " (pr-str lic)
                           "; only " (pr-str (vec (sort allowed-licenses))) " permitted. hotaru is an OPEN-PUBLICATION "
                           "commons — vendor-proprietary / patent-active / trade-secret recipes are excluded by "
                           "construction (no knowledge record produced); see ADR-2605265500 §2.") {:gate "G1"})))
    (let [stage (norm (get state "stage" (get cs "stage")))]
      (when-not (contains? allowed-stages stage)
        (throw (ex-info (str "unknown substrate stage " (pr-str stage) "; expected one of " (pr-str (vec (sort allowed-stages)))) {})))
      (let [cite (get state "source_cite" (get cs "source_cite"))]
        (when-not (seq cite)
          (throw (ex-info "G5 violation: open-publication record requires a :source-cite" {:gate "G5"})))
        {"cell_state" (assoc cs "source_license" lic "stage" stage "source_cite" cite
                             "maturity" (norm (get state "maturity" (get cs "maturity")))
                             "screened" true "phase" "screened")}))))

(defn transition-to-recorded [state]
  (let [cs (cell-state state)]
    (when-not (get cs "screened")
      (throw (ex-info "knowledge record requires a passed open-IP screen first (G1)" {:gate "G1"})))
    {"cell_state" (assoc cs "phase" "recorded"
                         "payload" {"procId" (get cs "proc_id") "stage" (get cs "stage")
                                    "sourceLicense" (get cs "source_license") "sourceCite" (get cs "source_cite")
                                    "maturity" (get cs "maturity") "screened" true "sourcing" (get cs "sourcing")})}))

(defn solve [_input-state]
  (throw (ex-info "hotaru R0 scaffold: activate commons_ingest via Council ADR (post-2606051200 ratification)" {:scaffold true})))
